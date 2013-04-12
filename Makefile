# Feel free to use, reuse and abuse the code in this file.

all: app

app: get-deps
	@./rebar compile
	@./mkcert.sh
	@erlc do_compile_rel.erl
	@./chkrel.bsh

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean
	@rm -f erl_crash.dump
	@rm -f do_compile_rel.beam

dist-clean: clean
