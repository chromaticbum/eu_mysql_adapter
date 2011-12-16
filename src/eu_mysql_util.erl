-module(eu_mysql_util).

-include_lib("eunit/include/eunit.hrl").

-export([
    create_table_sql/2
  ]).

create_table_sql(Table, Columns) ->
  list_to_binary(
    lists:concat(
      ["create table if not exists ", Table, " (",
        columns_sql(Columns), ");"]
    )
  ).

columns_sql(Columns) ->
  ColumnList = string:join(lists:map(fun(Column) -> column_sql(Column) end, Columns), ", ").

column_sql({Column, Type, Options}) ->
  string:join([column_sql({Column, Type}), options_string(Options)], " ");
column_sql({Column, string}) ->
  string:join([atom_to_list(Column), "varchar(255)"], " ");
column_sql({Column, Type}) ->
  string:join([atom_to_list(Column), atom_to_list(Type)], " ").

options_string(Options) ->
  string:join(
    lists:map(fun(Option) -> option_string(Option) end, Options),
    " "
  ).

option_string(primary) ->
  "primary key auto_increment".

% TESTS

create_table_sql_test() ->
  Sql = <<"create table if not exists players (id int primary key auto_increment, name varchar(255));">>,
  ?assertEqual(Sql,
    create_table_sql(players, [{id, int, [primary]}, {name, string}])).

columns_sql_test() ->
  ?assertEqual("id int primary key auto_increment, name varchar(255)",
    columns_sql([{id, int, [primary]}, {name, string}])).

option_string_test() ->
  ?assertEqual("primary key auto_increment",
    option_string(primary)).

column_sql_test() ->
  ?assertEqual("id int primary key auto_increment",
    column_sql({id, int, [primary]})),
  ?assertEqual("name varchar(255)",
    column_sql({name, string})).
