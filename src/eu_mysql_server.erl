-module(eu_mysql_server).
-behavior(gen_server).

-include_lib("emysql/include/emysql.hrl").
-include("eu_mysql_adapter.hrl").

-export([
    start_link/1,
    create/1
  ]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

-export([
    version/1,
    update_version/2,
    create_table/3,
    drop_table/2,
    add_column/3,
    drop_column/3
  ]).

% TODO: get rid
-export([
    db_info1/5
  ]).

-record(state, {
    db_info :: #db_info{},
    pool :: atom()
  }).

-spec start_link(DbInfo) -> {ok, Pid} when
  DbInfo :: #db_info{},
  Pid :: pid().
start_link(DbInfo) ->
  gen_server:start_link(?MODULE, [DbInfo], []).


-spec create(DbInfo) -> {ok, Pid} when
  DbInfo ::#db_info{},
  Pid :: pid().
create(DbInfo) ->
  eu_mysql_adapter_sup:start_child(DbInfo).


-spec init([any()]) -> {ok, State} | {stop, Reason} when
  State :: #state{},
  Reason :: atom().
init([DbInfo]) ->
  State = #state{
    db_info = DbInfo
  },

  case ensure_migration_table(State) of
    {ok, State2} ->
      {ok, State2};
    {error, Error} ->
      error_logger:error_msg("Database/migration table creation error: ~p~n", [Error]),
      {stop, database_creation}
  end.


-spec ensure_migration_table(State) -> {ok, State2} | {error, Error} when
  State :: #state{},
  State2 :: #state{},
  Error :: #error_packet{}.
ensure_migration_table(
  #state{
    db_info = #db_info{
      user = User,
      password = Password,
      host = Host,
      port = Port,
      database = Database
    }
  } = State
) ->
  ensure_database(State),
  Pool = mysql_pool_name(State),
  emysql:add_pool(Pool, 1,
    User, Password, Host, Port, Database, utf8),

  State2 = State#state{pool = Pool},
  case create_migration_table(State2) of
    ok ->
      {ok, State2};
    {error, Error} -> {error, Error}
  end.


-spec create_migration_table(State) -> ok | {error, Error} when
  State :: #state{},
  Error :: #error_packet{}.
create_migration_table(#state{pool = Pool}) ->
  Sql =
    <<"create table if not exists migrations (
    migration varchar(255) primary key)">>,
  case emysql:execute(Pool, Sql) of
    #ok_packet{} -> ok;
    Error -> {error, Error}
  end.


-spec ensure_database(State) -> ok | {error, Error} when
  State :: #state{},
  Error :: #error_packet{}.
ensure_database(
  #state{
    db_info = #db_info{
      user = User,
      password = Password,
      host = Host,
      port = Port
    }
  } = State
) ->
  Pool = mysql_pool_name(State),
  emysql:add_pool(Pool, 1,
    User, Password, Host, Port, "mysql", utf8),
  Result = create_database(State, Pool),
  emysql:remove_pool(Pool),
  Result.


-spec create_database(State, Pool) -> ok | {error, Error} when
  State :: #state{},
  Pool :: atom(),
  Error :: #error_packet{}.
create_database(
  #state{
    db_info = #db_info{database = Database}
  },
  Pool
) ->
  Sql = list_to_binary(lists:concat(["create schema if not exists ", Database])),
  case emysql:execute(Pool, Sql) of
    #result_packet{} -> ok;
    Error -> {error, Error}
  end.


-spec mysql_pool_name(State) -> Name when
  State :: #state{},
  Name :: atom.
mysql_pool_name(
  #state{
    db_info = #db_info{database = Database}
  }
) ->
  list_to_atom(lists:concat(["eu_mysql_", Database, "_pool"])).


-spec version(Pid) -> Version when
  Pid :: atom(),
  Version :: version().
version(Pid) ->
  gen_server:call(Pid, version).


-spec update_version(Pid, Version) -> ok when
  Pid :: pid(),
  Version :: version().
update_version(Pid, Version) ->
  gen_server:call(Pid, {update_version, Version}).

-spec create_table(Pid, Table, Columns) -> ok when
  Pid :: pid(),
  Table :: atom(),
  Columns :: columns().
create_table(Pid, Table, Columns) ->
  gen_server:call(Pid, {create_table, Table, Columns}).


-spec drop_table(Pid, Table) -> ok when
  Pid :: pid(),
  Table :: table().
drop_table(Pid, Table) ->
  gen_server:call(Pid, {drop_table, Table}).


-spec add_column(Pid, Table, Column) -> ok when
  Pid :: pid(),
  Table :: atom(),
  Column :: column().
add_column(Pid, Table, Column) ->
  gen_server:call(Pid, {add_column, Table, Column}).


-spec drop_column(Pid, Table, Column) -> ok when
  Pid :: pid(),
  Table :: table(),
  Column :: column_name().
drop_column(Pid, Table, Column) ->
  gen_server:call(Pid, {drop_column, Table, Column}).


handle_call(version, _From, #state{pool = Pool} = State) ->
  #result_packet{rows = Rows} = emysql:execute(Pool, migration_version, []),

  case Rows of
    [[Version]] -> {reply, binary_to_list(Version), State};
    [] -> {reply, "", State}
  end;

handle_call({update_version, Version}, _From, #state{pool = Pool} = State) ->
  emysql:execute(Pool, migration_update_version, [Version]),
  {reply, ok, State};

handle_call({create_table, Table, Columns}, _From, #state{pool = Pool} = State) ->
  emysql:execute(Pool, eu_mysql_util:create_table_sql(Table, Columns)),
  {reply, ok, State};

handle_call({drop_table, Table}, _From, #state{pool = Pool} = State) ->
  emysql:execute(Pool, eu_mysql_util:drop_table_sql(Table)),
  {reply, ok, State};

handle_call({add_column, Table, Column}, _From, #state{pool = Pool} = State) ->
  emysql:execute(Pool, eu_mysql_util:add_column_sql(Table, Column)),
  {reply, ok, State};

handle_call({drop_column, Table, Column}, _From, #state{pool = Pool} = State) ->
  emysql:execute(Pool, eu_mysql_util:drop_column_sql(Table, Column)),
  {reply, ok, State};

handle_call(_Data, _From, State) ->
  {reply, ok, State}.

handle_cast(_Data, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  {reply, ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% TESTS

db_info1(User, Password, Host, Port, Database) ->
  #db_info{
    adapter = mysql,
    user = User,
    password = Password,
    host = Host,
    port = Port,
    database = Database
  }.