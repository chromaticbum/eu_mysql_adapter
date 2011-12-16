-module(eu_mysql_adapter).
-behavior(gen_server).

-include_lib("emysql/include/emysql.hrl").

-export([
    start_link/5,
    create/5
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
    create_table/3
  ]).

-record(state, {
    user = "" :: string(),
    password = "" :: string(),
    host = "" :: string(),
    port = -1 :: integer(),
    database = "" :: string(),

    pool :: atom()
  }).

-type version() :: string().

-type column_name() :: atom().
-type column_type() ::
  int |
  float |
  string |
  timestamp |
  datetime.
-type column_option() ::
  primary.

-type column() ::
  {column_name(), column_type()} |
  {column_name(), column_type(), [column_option()]}.

-type columns() :: [column()].

-spec start_link(User, Password, Host, Port, Database) -> {ok, Pid} when
  User :: string(),
  Password :: string(),
  Host :: string(),
  Port :: integer(),
  Database :: string(),
  Pid :: pid().
start_link(User, Password, Host, Port, Database) ->
  gen_server:start_link(?MODULE, [User, Password, Host, Port, Database], []).


-spec create(User, Password, Host, Port, Database) -> {ok, Pid} when
  User :: string(),
  Password :: string(),
  Host :: string(),
  Port :: integer(),
  Database :: string(),
  Pid :: pid().
create(User, Password, Host, Port, Database) ->
  eu_mysql_adapter_sup:start_child(User, Password, Host, Port, Database).


-spec init([any()]) -> {ok, State} | {error, Reason} when
  State :: #state{},
  Reason :: atom().
init([User, Password, Host, Port, Database]) ->
  State = #state{
    user = User,
    password = Password,
    host = Host,
    port = Port,
    database = Database
  },

  case ensure_migration_table(State) of
    {ok, State2} ->
      {ok, State2};
    {error, Error} ->
      error_logger:error_msg("Database/migration table creation error: ~p~n", [Error]),
      {stop, database_creation}
  end.


-spec ensure_migration_table(State) -> State2 | {error, Error} when
  State :: #state{},
  State2 :: #state{},
  Error :: #error_packet{}.
ensure_migration_table(
  #state{
    user = User,
    password = Password,
    host = Host,
    port = Port,
    database = Database
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
    Error -> {error, Error}
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
    user = User,
    password = Password,
    host = Host,
    port = Port
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
  #state{database = Database},
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
mysql_pool_name(#state{database = Database}) ->
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


