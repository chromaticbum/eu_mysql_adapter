-module(eu_mysql_adapter_app).
-behavior(application).

-export([
    start/2,
    stop/1
  ]).

-spec start(any(), any()) -> {ok, Pid} when
  Pid :: pid().
start(_Type, _Args) ->
  prepare_statements(),
  eu_mysql_adapter_sup:start_link().

-spec prepare_statements() -> ok.
prepare_statements() ->
  emysql:prepare(migration_version, <<"select migration from migrations order by migration desc limit 1;">>),
  emysql:prepare(migration_update_version, <<"insert into migrations(migration) values(?);">>),

  ok.


-spec stop(any()) -> ok.
stop(_S) ->
  ok.
