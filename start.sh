#!/bin/sh
erl -name node@host -pa ebin -pa deps/*/ebin -s esysman -setcookie some-cookie\
	-eval "io:format(\"~n~nThe following sites are available:~n\")." \
	-eval "io:format(\"* http://localhost:8080/esysman~n\")." \
    -eval "io:format(\"* https://localhost:8443/esysman~n\")."
