PROJECT = esysman

DEPS = cowboy epgsql
dep_cowboy = git https://github.com/ninenines/cowboy 2.8.0 
dep_epgsql = git https://github.com/wg/epgsql.git master

include erlang.mk
include extra.mk
