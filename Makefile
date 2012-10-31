all: compile


compile:
	-@mkdir -p ebin
	erlc -o ebin src/*.erl

clean:
	-rm ebin/*.beam

run:
	erl -boot start_sasl -pa ebin -eval 'chatroom:start_link({global,chatroom}).'

.PHONY: all compile clean run start

