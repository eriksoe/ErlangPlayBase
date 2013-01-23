-module(drunk_bot).

%% API
-export([start_link/1]).

%% For spawning
-export([init/1]).

-define(COMMAND, "!drunk ").

start_link(ChatRoom) when is_pid(ChatRoom) ->
    proc_lib:start(?MODULE, init, [ChatRoom]).

init(ChatRoom) ->
    proc_lib:init_ack(self()),
    ChatRoom ! {join, self()},
    loop(ChatRoom).

loop(ChatRoom) ->
    receive
        ?COMMAND ++ Msg ->
            Words = string:tokens(Msg, " "),
            Fun = fun(M) ->
                Left = string:left(M, 1),
                Right = string:sub_string(M, 2),
                Left ++ "j" ++ Right
            end,
            Drunkmsg = string:join(lists:map(Fun, Words), " "),
            ChatRoom ! Drunkmsg;
        _ ->
            ok
    end,
    loop(ChatRoom).