
Basis project for Erlang GOTONight;
===================================

Searchable documentation site: http://www.erlang.org/erldoc


All commands below assume you are standing in the root directory of
ErlangPlayBase repository.

```
  $> means OS shell
  >  means erlang shell
```

Quick Start
-----------

```
  $> git clone git://github.com/eriksoe/ErlangPlayBase.git
  $> cd ErlangPlayBase
  $> erl -make
  $> erl -pa ebin -boot start_sasl -name <YOUR-NAME>@<IP-ADDRESS> -setcookie omnomnom
  > net_adm:ping('chatserver@<IP...>').
  > chat_client:start(chatroom:global_chatroom()).
```

Command commands
----------------


Clone todays project:

```
  $> git clone git://github.com/eriksoe/ErlangPlayBase.git
```

Compilation from command line

```
  $> erl -make
```
Compilation from the erlang shell

```
  > make:all([load]).
```

Single-node shell:

```
  $> erl -pa ebin #maybe also -boot start_sasl
```

Distributed shell:

```
  $> erl -pa ebin -boot start_sasl -name <YOUR-NAME>@<IP-ADDRESS> -setcookie omnomnom
```

Connecting to the Erlang cluster and starting the chat client:

```
  $> erl -pa ebin -boot start_sasl -name <YOUR-NAME>@<IP-ADDRESS> -setcookie omnomnom
  > net_adm:ping('chatserver@<IP...>').
  > nodes(). % should return a list of visible node names
  > CR = chatroom:global_chatroom().
  > chat_client:start(CR).
```