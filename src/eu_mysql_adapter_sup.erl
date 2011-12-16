-module(eu_mysql_adapter_sup).
-behavior(supervisor).

-export([
    start_link/0,
    start_child/5
  ]).

-export([
    init/1
  ]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, Pid} when
  Pid :: pid().
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_child(User, Password, Host, Port, Database) -> {ok, Pid} when
  User :: string(),
  Password :: string(),
  Host :: string(),
  Port :: integer(),
  Database :: string(),
  Pid :: pid().
start_child(User, Password, Host, Port, Database) ->
  supervisor:start_child(?SERVER, [
      User, Password, Host, Port, Database
    ]).

-spec init([]) -> {ok, {RestartStrategy, Children}} when
  RestartStrategy :: any(),
  Children :: any().
init([]) ->
  Server = {eu_mysql_adapter, {eu_mysql_adapter, start_link, []},
    temporary, 2000, worker, [eu_mysql_adapter]},
  Children = [Server],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
