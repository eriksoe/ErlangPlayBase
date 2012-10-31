-module(console_logger_bot).

-export([start_link/1, init/1]).

start_link(Chatroom) when is_pid(Chatroom) ->
    proc_lib:start_link(?MODULE, init, [Chatroom]).

init(Chatroom) ->
    proc_lib:init_ack(self()),
    Chatroom ! {join, self()},
    loop().

loop() ->
    receive M ->
            io:format("~p\n", [M])
    end,
    loop().
