-module(chat_client).

-export([start/1]).

%% Synopsis:
%% chat_client:start(chatroom:local_chatroom()).

start(Chatroom) ->
    OutPid = proc_lib:spawn_link(fun() -> output_init(Chatroom) end),
    input_loop(Chatroom, OutPid).

input_loop(Chatroom, OutPid) ->
    case io:get_line(standard_io, "> ") of
        eof -> ok;
        "/quit\n" ->
            exit(OutPid, normal),
            ok;
        Line0 ->
            Line = string:strip(Line0, right, $\n),
            (Line == []) orelse (Chatroom ! Line),
            input_loop(Chatroom, OutPid)
    end.

output_init(Chatroom) ->
    Chatroom ! {join, self()},
    output_loop().

output_loop() ->
    receive
        M ->
            case is_string(M) of
                true -> io:format("~s\n", [M]);
                false -> io:format("~p\n", [M])
            end
    end,
    output_loop().

is_string([]) -> true;
is_string([H|T]) ->
    is_integer(H) andalso
        H>=0 andalso
        H=<16#FFFF andalso
        is_string(T);
is_string(_) -> false.
