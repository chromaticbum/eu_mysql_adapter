-module(eu_mysql_adapter_sup).
-behavior(supervisor).

-include("eu_mysql_adapter.hrl").

-export([
    start_link/0,
    start_child/1
  ]).

-export([
    init/1
  ]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, Pid} when
  Pid :: pid().
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_child(DbInfo) -> {ok, Pid} when
  DbInfo :: #db_info{},
  Pid :: pid().
start_child(DbInfo) ->
  supervisor:start_child(?SERVER, [DbInfo]).

-spec init([]) -> {ok, {RestartStrategy, Children}} when
  RestartStrategy :: any(),
  Children :: any().
init([]) ->
  Server = {eu_mysql_server, {eu_mysql_server, start_link, []},
    temporary, 2000, worker, [eu_mysql_server]},
  Children = [Server],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
