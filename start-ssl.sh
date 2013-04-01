#!/bin/sh

erl -boot /path/to/esysman_ssl-16b -proto_dist inet_tls \
	-ssl_dist_opt server_certfile "/path/to/ssl/cert.pem" \
	-ssl_dist_opt server_keyfile "/path/to/ssl/key.pem" \
	-ssl_dist_opt server_secure_renegotiate true client_secure_renegotiate true \
	-name node@host -pa ebin -pa deps/*/ebin -s esysman \
	-eval "io:format(\"~n~nThe following examples are available:~n\")." \
	-eval "io:format(\"* Hello world: http://localhost:port/esysman~n\")." \
    -eval "io:format(\"* Hello World (ssl): https://localhost:port/esysman~n\")."