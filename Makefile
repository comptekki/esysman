PROJECT = esysman

DEPS = cowboy epgsql
dep_cowboy = pkg://cowboy 2.2.1
dep_epgsql = git://github.com/wg/epgsql.git master

include erlang.mk
include extra.mk
