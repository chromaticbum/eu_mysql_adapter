-type sql() :: string().
-type sql_binary() :: binary().

-type version() :: string().

-type table() :: atom().

-type column_name() :: atom().
-type column_type() ::
  int |
  float |
  string |
  timestamp |
  datetime.
-type column_option() ::
  primary.
-type column_options() :: [column_option()].

-type column() ::
  {column_name(), column_type()} |
  {column_name(), column_type(), column_options()}.

-type columns() :: [column()].

