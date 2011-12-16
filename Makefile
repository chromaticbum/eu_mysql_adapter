compile:
	rebar compile

test: compile
	erl -noshell -pa ebin -eval "application:start(crypto), application:start(emysql), application:start(eu_mysql_adapter), eunit:test([eu_mysql_util], [verbose])" -s init stop

console: compile
	erl -pa ebin -eval "application:start(crypto), application:start(emysql), application:start(eu_mysql_adapter)."
