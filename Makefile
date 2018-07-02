PROJECT = esysman

DEPS = cowboy epgsql
#dep_cowboy = pkg://cowboy 2.2.2
dep_cowboy = git https://github.com/ninenines/cowboy 2.4.0 
dep_epgsql = git https://github.com/wg/epgsql.git master

include erlang.mk
include extra.mk
