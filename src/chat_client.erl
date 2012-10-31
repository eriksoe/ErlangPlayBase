-module(chat_client).

-export([start/1]).

%% Synopsis:
%% chat_client:start(chatroom:local_chatroom()).

start(Chatroom) ->
    Ref = make_ref(),
    OutPid = proc_lib:spawn_link(fun() -> output_init(Chatroom,Ref) end),
    input_loop(Chatroom, OutPid, Ref).

input_loop(Chatroom, OutPid, Ref) ->
    case io:get_line(standard_io, "> ") of
        eof -> ok;
        "/quit\n" ->
            OutPid ! {stop,Ref},
            ok;
        Line0 ->
            Line = string:strip(Line0, right, $\n),
            (Line == []) orelse (Chatroom ! Line),
            input_loop(Chatroom, OutPid, Ref)
    end.

output_init(Chatroom,Ref) ->
    Chatroom ! {join, self()},
    output_loop(Ref).

output_loop(Ref) ->
    receive
        {stop,Ref} ->
            io:format("Left the chatroom.\n"),
            ok;
        M ->
            case is_string(M) of
                true -> io:format("~s\n", [M]);
                false -> io:format("~p\n", [M])
            end,
            output_loop(Ref)
    end.

is_string([]) -> true;
is_string([H|T]) ->
    is_integer(H) andalso
        H>=0 andalso
        H=<16#FFFF andalso
        is_string(T);
is_string(_) -> false.
