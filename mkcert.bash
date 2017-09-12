#!/bin/bash
if [ -a ./priv/ssl/server.crt -o -a ./priv/ssl/server.key ]
then

echo
echo "priv/ssl/server.crt and/or priv/ssl/server.key already exist."
echo "Delete priv/ssl/server.crt and priv/ssl/server.key to create new ones."
echo

else

mkdir -p priv/ssl
umask 077 && touch priv/ssl/server.key priv/ssl/server.crt
openssl req -new -newkey rsa:4096 -days 365 -nodes -x509 -keyout priv/ssl/server.key -out priv/ssl/server.crt
echo
echo
echo "Done creating key and cert..."
echo

fi
