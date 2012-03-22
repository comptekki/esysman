# Feel free to use, reuse and abuse the code in this file.

all: app

app: get-deps
	@./rebar compile
	@./mkcert.sh

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean
	rm -f erl_crash.dump

dist-clean: clean
