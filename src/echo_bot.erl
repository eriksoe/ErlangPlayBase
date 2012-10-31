-module(echo_bot).

%% Synopsis:
%% echo_bot:start_link(chatroom:local_chatroom()).

%% Public API
-export([start_link/1]).

%% For spawning
-export([init/1]).

-define(COMMAND, "!echo ").

start_link(ChatRoom) when is_pid(ChatRoom) ->
    proc_lib:start_link(?MODULE, init, [ChatRoom]).

init(ChatRoom) ->
    proc_lib:init_ack(self()),
    ChatRoom ! {join, self()},
    loop(ChatRoom).

loop(ChatRoom) ->
    receive
        ?COMMAND ++ Msg ->
            ChatRoom ! Msg;
        _ ->
            ok
    end,
    loop(ChatRoom).
