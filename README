
Basis project for Erlang GOTONight;
===================================

Searchable documentation site: http://www.erlang.org/erldoc


All commands below assume you are standing in the root directory of ErlangPlayBase.

  $> means OS shell
  >  means erlang shell

Clone todays project:

  $> git clone git://github.com/eriksoe/ErlangPlayBase.git

Command line compilation:

  $> erl -make

Erl compilation:

  > make:all([load]).

Single-node shell:

  $> erl -pa ebin #maybe also -boot start_sasl

Distributed shell:

  $> erl -pa ebin -boot start_sasl -name <your-name>@<IP-address> -setcookie omnomnom

Connecting to the Erlang cluster and starting the chat client:

  $> erl -pa ebin -boot start_sasl -name <your-name>@<IP-address> -setcookie omnomnom
  > net_adm:ping('chatserver@<IP...>').
  > nodes(). # should return a list of visible node names
  > CR = chatroom:global_chatroom().
  > chat_client:start(CR).
