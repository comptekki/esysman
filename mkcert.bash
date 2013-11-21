#!/bin/bash
if [ -a ./priv/ssl/cert.pem -o -a ./priv/ssl/key.pem ]
then

echo
echo "priv/ssl/cert.pem and/or priv/ssl/key.pem already exist."
echo "Delete priv/ssl/cert.pem or priv/ssl/key.pem to create new ones."
echo

else

mkdir -p priv/ssl
umask 077 && touch priv/ssl/key.pem priv/ssl/cert.pem
openssl genrsa 2048 > priv/ssl/key.pem
openssl req -new -x509 -nodes -sha1 -days 3650 -out priv/ssl/cert.pem -key priv/ssl/key.pem < blank
echo
echo
echo "Done creating cert..."
echo

fi