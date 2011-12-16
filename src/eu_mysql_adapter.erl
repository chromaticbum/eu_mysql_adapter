-module(eu_mysql_adapter).
-behavior(gen_server).

-include_lib("emysql/include/emysql.hrl").

-export([
    start_link/5
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
    ensure_database/1
  ]).

-record(state, {
    user = "" :: string(),
    password = "" :: string(),
    host = "" :: string(),
    port = -1 :: integer(),
    database = "" :: string(),

    pool :: atom()
  }).

-spec start_link(User, Password, Host, Port, Database) -> {ok, Pid} when
  User :: string(),
  Password :: string(),
  Host :: string(),
  Port :: integer(),
  Database :: string(),
  Pid :: pid().
start_link(User, Password, Host, Port, Database) ->
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
    ok -> {ok, State};
    {error, _Error} -> {stop, database_creation}
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
  Pool = mysql_pool_name(State),
  emysql:add_pool(Pool, 1,
    User, Password, Host, Port, Database, utf8),

  State2 = State#state{pool = Pool},

  case create_migration_table(State2) of
    #result_packet{} -> State2;
    Error -> {error, Error}
  end.


-spec create_migration_table(State) -> ok | {error, Error} when
  State :: #state{},
  Error :: #error_packet{}.
create_migration_table(#state{pool = Pool} = State) ->
  ensure_database(State),
  Sql =
    <<"create table if not exists migrations (
    migration varchar(255) primary key)">>,
  case emysql:execute(Pool, Sql) of
    #result_packet{} -> ok;
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


