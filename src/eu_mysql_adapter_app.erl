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
  emysql:prepare(migration_version, <<"select version from migrations order by version desc limit 1;">>),
  emysql:prepare(store_instruction, <<"insert into migrations(version, file, `table`, `column`, instruction) values(?, ?, ?, ?, ?);">>),
  emysql:prepare(delete_instruction, <<"delete from migrations where version = ? and file = ? and `table` = ? and `column` = ?;">>),
  emysql:prepare(restore_table_instructions, <<"select instruction from migrations where version < ? and table = ? order by created_at asc;">>),

  ok.


-spec stop(any()) -> ok.
stop(_S) ->
  ok.
