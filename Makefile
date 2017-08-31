PROJECT = esysman

DEPS = cowboy epgsql
dep_cowboy = pkg://cowboy 2.0.0-rc.1
# 2.0.0-pre.4
# 2.0.0-rc.1
# 1.1.2
dep_epgsql = git://github.com/wg/epgsql.git master

include erlang.mk
include extra.mk
