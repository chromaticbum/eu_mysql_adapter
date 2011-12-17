-module(eu_mysql).

-include_lib("eulogy/include/eulogy.hrl").
-include("eu_mysql_adapter.hrl").

% eu_mysql_adapter API exports
-export([
    create/1, create/5,
    stop/1,
    version/1,
    update_version/2,
    create_table/3,
    drop_table/2,
    add_column/3,
    drop_column/3
  ]).


-spec create(User, Password, Host, Port, Database) -> Adapter when
  User :: string(),
  Password :: string(),
  Host :: string(),
  Port :: integer(),
  Database :: string(),
  Adapter :: #eu_mysql{}.
create(User, Password, Host, Port, Database) ->
  DbInfo = #db_info{
    adapter = mysql,
    user = User,
    password = Password,
    host = Host,
    port = Port,
    database = Database
  },
  create(DbInfo).


-spec create(DbInfo) -> Adapter when
  DbInfo :: #db_info{},
  Adapter :: #eu_mysql{}.
create(DbInfo) ->
  {ok, Pid} = eu_mysql_server:create(DbInfo),
  #eu_mysql{pid = Pid}.


-spec stop(Adapter) -> ok when
  Adapter :: #eu_mysql{}.
stop(#eu_mysql{pid = Pid}) ->
  eu_mysql_server:stop(Pid).


-spec version(Adapter) -> Version when
  Adapter :: #eu_mysql{},
  Version :: version().
version(#eu_mysql{pid = Pid}) ->
  eu_mysql_server:version(Pid).


-spec update_version(Adapter, Version) -> ok when
  Adapter :: #eu_mysql{},
  Version :: version().
update_version(#eu_mysql{pid = Pid}, Version) ->
  eu_mysql_server:update_version(Pid, Version).


-spec create_table(Adapter, Table, Columns) -> ok when
  Adapter :: #eu_mysql{},
  Table :: table(),
  Columns :: columns().
create_table(#eu_mysql{pid = Pid}, Table, Columns) ->
  eu_mysql_server:create_table(Pid, Table, Columns).


-spec drop_table(Adapter, Table) -> ok when
  Adapter :: #eu_mysql{},
  Table :: table().
drop_table(#eu_mysql{pid = Pid}, Table) ->
  eu_mysql_server:drop_table(Pid, Table).


-spec add_column(Adapter, Table, Column) -> ok when
  Adapter :: #eu_mysql{},
  Table :: table(),
  Column :: column().
add_column(#eu_mysql{pid = Pid}, Table, Column) ->
  eu_mysql_server:add_column(Pid, Table, Column).


-spec drop_column(Adapter, Table, Column) -> ok when
  Adapter :: #eu_mysql{},
  Table :: table(),
  Column :: column_name().
drop_column(#eu_mysql{pid = Pid}, Table, Column) ->
  eu_mysql_server:drop_column(Pid, Table, Column).
