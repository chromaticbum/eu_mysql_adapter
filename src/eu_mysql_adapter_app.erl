-module(eu_mysql_adapter_app).
-behavior(application).

-export([
    start/2,
    stop/1
  ]).

-spec start(any(), any()) -> {ok, Pid} when
  Pid :: pid().
start(_Type, _Args) ->
  eu_mysql_adapter_sup:start_link().

-spec stop(any()) -> ok.
stop(_S) ->
  ok.
