PROJECT = esysman
PROJECT_DESCRIPTION = esysman
PROJECT_VERSION = 1.9

DEPS = cowboy epgsql
REL_DEPS += relx

dep_cowboy = git https://github.com/ninenines/cowboy 2.13.0
dep_epgsql = git https://github.com/wg/epgsql.git master

include erlang.mk
include extra.mk
