all: compile


compile:
	-@mkdir -p ebin
	erlc -o ebin src/*.erl
	erlc -o test test/*.erl

clean:
	-rm ebin/*.beam test/*.beam

run: compile
	erl -boot start_sasl -pa ebin -eval 'chatroom:start_link({global,chatroom}).'

test: compile
	erl -noshell -pa ebin test -eval 'chatroom_crude_test:test(), init:stop().'


.PHONY: all compile clean run test

