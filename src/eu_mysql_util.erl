-module(eu_mysql_util).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eulogy/include/eulogy.hrl").
-include("eu_mysql_adapter.hrl").

-export([
    create_table_sql/2,
    drop_table_sql/1,
    add_column_sql/2,
    drop_column_sql/2
  ]).


-spec create_table_sql(Table, Columns) -> Sql when
  Table :: table(),
  Columns :: columns(),
  Sql :: sql_binary().
create_table_sql(Table, Columns) ->
  list_to_binary(
    lists:concat(
      ["create table ", Table, " (",
        columns_sql(Columns), ");"]
    )
  ).


-spec drop_table_sql(Table) -> Sql when
  Table :: table(),
  Sql :: sql_binary().
drop_table_sql(Table) ->
  list_to_binary(
    lists:concat(
      ["drop table ", Table, ";"]
    )
  ).


-spec add_column_sql(Table, Column) -> Sql when
  Table :: table(),
  Column :: column(),
  Sql :: sql_binary().
add_column_sql(Table, Column) ->
  list_to_binary(
    lists:concat(
      ["alter table ", Table, " add column ", column_sql(Column), ";"]
    )
  ).


-spec drop_column_sql(Table, Column) -> Sql when
  Table :: table(),
  Column :: column_name(),
  Sql :: sql_binary().
drop_column_sql(Table, Column) ->
  list_to_binary(
    lists:concat(
      ["alter table ", Table, " drop column ", Column, ";"]
    )
  ).


-spec columns_sql(Columns) -> Sql when
  Columns :: columns(),
  Sql :: sql().
columns_sql(Columns) ->
  string:join(lists:map(fun(Column) -> column_sql(Column) end, Columns), ", ").


-spec column_sql(Column) -> Sql when
  Column :: column(),
  Sql :: sql().
column_sql({Column, Type, Options}) ->
  string:join([column_sql({Column, Type}), options_string(Options)], " ");
column_sql({Column, string}) ->
  string:join([atom_to_list(Column), "varchar(255)"], " ");
column_sql({Column, Type}) ->
  string:join([atom_to_list(Column), atom_to_list(Type)], " ").


-spec options_string(Options) -> Sql when
  Options :: column_options(),
  Sql :: sql().
options_string(Options) ->
  string:join(
    lists:map(fun(Option) -> option_string(Option) end, Options),
    " "
  ).


-spec option_string(Option) -> Sql when
  Option :: column_option(),
  Sql :: sql().
option_string(primary) ->
  "primary key";
option_string(unique) ->
  "unique key";
option_string(auto_increment) ->
  "auto_increment";
option_string(primary_id) ->
  "primary key auto_increment".

% TESTS

create_table_sql_test() ->
  Sql = <<"create table players (id int primary key auto_increment, name varchar(255));">>,
  ?assertEqual(Sql,
    create_table_sql(players, [{id, int, [primary_id]}, {name, string}])).

add_column_sql_test() ->
  Sql = <<"alter table players add column country varchar(255) unique key;">>,
  ?assertEqual(Sql,
    add_column_sql(players, {country, string, [unique]})).

columns_sql_test() ->
  ?assertEqual("id int primary key auto_increment, name varchar(255)",
    columns_sql([{id, int, [primary_id]}, {name, string}])).

option_string_test() ->
  ?assertEqual("primary key",
    option_string(primary)),
  ?assertEqual("unique key",
    option_string(unique)),
  ?assertEqual("auto_increment",
    option_string(auto_increment)),
  ?assertEqual("primary key auto_increment",
    option_string(primary_id)).

column_sql_test() ->
  ?assertEqual("id int primary key auto_increment",
    column_sql({id, int, [primary_id]})),
  ?assertEqual("name varchar(255)",
    column_sql({name, string})).
