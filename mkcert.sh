#!/bin/sh
blank="US\n
some-state\n
some-city\n
some-org\n
some-unit\n
some-name\n
some-address\n
\n
"
if [ ! -d priv ] ; then
echo
echo Making SSL Certficates
echo
mkdir -p priv/ssl
umask 077 && touch priv/ssl/key.pem priv/ssl/cert.pem
openssl genrsa 2048 > priv/ssl/key.pem
echo $blank | openssl req -new -x509 -nodes -sha1 -days 3650 -key priv/ssl/key.pem > priv/ssl/cert.pem
echo
fi