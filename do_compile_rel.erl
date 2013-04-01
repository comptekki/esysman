-module(do_compile_rel).

-export([start/0]).

% compile rel file

start() ->
	systools:make_script("esysman_ssl").
