#!/bin/sh
erl -name node@host -pa /usr/local/lib/ebin -s ecom -setcookie some-cookie
