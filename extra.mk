certs:
	@./mkcert.bash

cleane:
	rm -f erl_crash.dump
	rm -f *.*~
	rm -f src/*.*~
	rm -f *~
	rm -f config/*.*~

clna: clean cleane
	rm -rf deps
	rm -rf _rel
	rm -f relx
	rm -f .erlang.mk.packages.*