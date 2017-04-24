PROJECT = esysman

DEPS = cowboy epgsql
dep_cowboy = pkg://cowboy 1.1.2
dep_epgsql = git://github.com/wg/epgsql.git master

include erlang.mk
include extra.mk
