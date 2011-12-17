-type sql() :: string().
-type sql_binary() :: binary().

% Records
-record(eu_mysql, {
    pid :: pid()
  }).
