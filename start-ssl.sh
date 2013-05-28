#!/bin/sh

erl -boot /usr/local/src/esysman/esysman_ssl -proto_dist inet_tls \
	-ssl_dist_opt server_certfile "/usr/local/src/esysman/priv/ssl/cert.pem" \
	-ssl_dist_opt server_keyfile "/usr/local/src/esysman/priv/ssl/key.pem" \
	-ssl_dist_opt server_secure_renegotiate true client_secure_renegotiate true \
	-name node@host -pa ebin -pa deps/*/ebin -s esysman \
	-eval "io:format(\"~n~nThe following examples are available:~n\")." \
	-eval "io:format(\"* Hello world: http://localhost:port/esysman~n\")." \
    -eval "io:format(\"* Hello World (ssl): https://localhost:port/esysman~n\")."