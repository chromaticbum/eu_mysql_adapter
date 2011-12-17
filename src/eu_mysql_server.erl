-module(eu_mysql_server).
-behavior(gen_server).

-include_lib("emysql/include/emysql.hrl").
-include_lib("eulogy/include/eulogy.hrl").
-include("eu_mysql_adapter.hrl").

-export([
    start_link/1,
    create/1,
    stop/1
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
    store_instruction/3,
    delete_instruction/3,

    create_table/3,
    drop_table/2,
    add_column/3,
    drop_column/3,

    restore_table_instructions/3,
    restore_column_instruction/4
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


-spec stop(Pid) -> ok when
  Pid :: pid().
stop(Pid) ->
  gen_server:call(Pid, stop).


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
    version varchar(255) not null,
    file varchar(255) not null,
    created_at timestamp not null default current_timestamp,
    `table` varchar(255) not null,
    `column` varchar(255) not null default '',
    `instruction` text not null,
    primary key (version, file, created_at)
  );">>,
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


-spec store_instruction(Pid, Migration, Instruction) -> ok when
  Pid :: pid(),
  Migration :: migration(),
  Instruction :: migration_instruction().
store_instruction(Pid, Migration, Instruction) ->
  gen_server:call(Pid, {store_instruction, Migration, Instruction}).


-spec delete_instruction(Pid, Migration, Instruction) -> ok when
  Pid :: pid(),
  Migration :: migration(),
  Instruction :: migration_instruction().
delete_instruction(Pid, Migration, Instruction) ->
  gen_server:call(Pid, {delete_instruction, Migration, Instruction}).


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


-spec restore_table_instructions(Pid, Version, Table) -> [term()] when
  Pid :: pid(),
  Version :: version(),
  Table :: table().
restore_table_instructions(Pid, Version, Table) ->
  gen_server:call(Pid, {restore_table_instructions, Version, Table}).


-spec restore_column_instruction(Pid, Version, Table, Column) -> term() when
  Pid :: pid(),
  Version :: version(),
  Table :: table(),
  Column :: column_name().
restore_column_instruction(Pid, Version, Table, Column) ->
  gen_server:call(Pid, {restore_column_instruction, Version, Table, Column}).


handle_call(version, _From, #state{pool = Pool} = State) ->
  #result_packet{rows = Rows} = emysql:execute(Pool, migration_version, []),

  case Rows of
    [[Version]] -> {reply, binary_to_list(Version), State};
    [] -> {reply, "", State}
  end;

handle_call({store_instruction, Migration, Instruction}, _From, #state{pool = Pool} = State) ->
  #migration{
    version = Version,
    file = File
  } = Migration,
  {Table, Column, Instruction2} = extract_instruction(Instruction),
  emysql:execute(Pool, store_instruction, [Version, File, Table, Column, lists:concat([Instruction2, "."])]),
  {reply, ok, State};

handle_call({delete_instruction, Migration, Instruction}, _From, #state{pool = Pool} = State) ->
  #migration{
    version = Version,
    file = File
  } = Migration,
  {Table, Column, _Instruction} = extract_instruction(Instruction),
  emysql:execute(Pool, delete_instruction, [Version, File, Table, Column]),
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

handle_call({restore_table_instructions, Version, Table}, _From, #state{pool = Pool} = State) ->
  #result_packet{rows = Rows} = emysql:execute(Pool, restore_table_instructions, [Version, Table]),
  {reply, encode_instruction_rows(Rows), State};

handle_call({restore_column_instruction, Version, Table, Column}, _From, #state{pool = Pool} = State) ->
  #result_packet{rows = [[Row]]} = emysql:execute(Pool, restore_column_instruction, [Version, Table, Column]),
  {reply, encode_term(Row), State};

handle_call(stop, _From, #state{pool = Pool} = State) ->
  emysql:remove_pool(Pool),
  {stop, normal, ok, State};

handle_call(_Data, _From, State) ->
  {reply, ok, State}.


encode_instruction_rows(Rows) ->
  lists:map(fun([Row]) -> encode_term(Row) end, Rows).


encode_term(Term) ->
  {ok, Tokens, _} = erl_scan:string(binary_to_list(Term)),
  {ok, Term2} = erl_parse:parse_term(Tokens),
  Term2.


-spec extract_instruction(Instruction) -> {string(), string(), string()} when
  Instruction :: migration_instruction().
extract_instruction({create_table, Table, _Columns} = Instruction) ->
  {atom_to_list(Table), "", convert_term(Instruction)};
extract_instruction({drop_table, Table} = Instruction) ->
  {atom_to_list(Table), "", convert_term(Instruction)};
extract_instruction({add_column, Table, Column} = Instruction) ->
  {atom_to_list(Table), atom_to_list(element(1, Column)), convert_term(Instruction)};
extract_instruction({drop_column, Table, Column} = Instruction) ->
  {atom_to_list(Table), atom_to_list(Column), convert_term(Instruction)};
extract_instruction({restore_table, Table} = Instruction) ->
  {atom_to_list(Table), "", convert_term(Instruction)};
extract_instruction({restore_column, Table, Column} = Instruction) ->
  {atom_to_list(Table), atom_to_list(Column), convert_term(Instruction)}.


-spec convert_term(any()) -> string().
convert_term(Term) ->
  lists:flatten(io_lib:format("~p", [Term])).


handle_cast(_Data, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  {reply, ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

