all: compile


compile:
	-@mkdir -p ebin
	erlc -o ebin src/*.erl
	erlc -o test test/*.erl

clean:
	-rm ebin/*.beam test/*.beam

shell:
	erl -sname chat -pa ebin

start: compile
	erl -boot start_sasl -sname chat -pa ebin -eval 'chatroom:start_link({local,chatroom}).'

start_global: compile
	erl -boot start_sasl -sname chat -pa ebin -eval 'chatroom:start_link({global,chatroom}).'

test: compile
	erl -noshell -pa ebin test -eval 'chatroom_crude_test:test(), init:stop().'


.PHONY: all compile clean start test

