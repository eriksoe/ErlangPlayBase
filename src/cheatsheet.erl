-module(cheatsheet).

-compile([export_all]).

receive_one_message() ->
    receive
        M ->
            io:format("Received some message: ~p~n",[M])
    end.

receive_one_message_timeout() ->
    receive
        M ->
            io:format("Received some message: ~p~n",[M])
    after
        %% 1 seconds
        1000 ->
            io:format("Tired of waiting~n"),
            timeout
    end.

spawn_process() ->
    io:format("In parent, before: pid=~p~n",[self()]),
    spawn_link(fun a_process/0),
    io:format("In parent, after: pid=~p~n",[self()]).

a_process() ->
    io:format("In child: pid=~p~n",[self()]).

case_exp(Input) ->
    case Input of
        one ->
            {ok, one};
        "two" ->
            {ok, two};
        <<"many">> ->
            {ok, many};
        Term ->
            {error, {unknown_term, Term}}
    end.

printing() ->
    io:format("Integer: ~b~n",[42]),
    io:format("Integer (binary): ~.2b~n",[42]),
    io:format("Integer (hex): ~.16b~n",[42]),
    io:format("Float: ~f~n", [math:pi()]),
    io:format("Float (3 decimals): ~.3f~n", [math:pi()]),
    io:format("Strings: ~s~n",["A string"]),
    io:format("Any term: ~p~n",[[one, {a, tuple}, "three"]]),
    lists:foreach(fun(X) -> io:format("~10b\t\~10b\t~10b\n", [X,X*X,X*X*X]) end, lists:seq(1,10)),
    ok.

regexp() ->
    S = "a needle contains iron",
    {match, [Word]}         = re:run(S, "needle\\s+(\\w+)",           [{capture, all_but_first, list}]),
    {match, [Word1, Word2]} = re:run(S, "needle\\s+(\\w+)\\s+(\\w+)", [{capture, all_but_first, list}]),
    io:format("Sentence: ~p~n", [S]),
    io:format("The word after needle: ~p~n",[Word]),
    io:format("The 2 words after needle: ~p ~p~n",[Word1, Word2]).

some_reflection() ->
    Exports = ?MODULE:module_info(exports),
    lists:foreach(fun({module_info,_}) ->
                          ok;
                     ({some_reflection=Fun, 0}) ->
                          io:format("===== Calling ~p/0~n",[Fun]),
                          io:format("      On second thought, I won't.  Endless recursion and such~n");
                     ({receive_one_message=Fun, 0}) ->
                          io:format("===== Calling ~p/0~n",[Fun]),
                          io:format("      Sending a message to self() first, though~n"),
                          self() ! hello,
                          ?MODULE:Fun();
                     ({Fun, 0}) ->
                          io:format("===== Calling ~p/0~n",[Fun]),
                          ?MODULE:Fun();
                     ({Fun, 1}) ->
                          io:format("===== Calling ~p/1~n",[Fun]),
                          ?MODULE:Fun("two");
                     ({Fun, N}) ->
                          io:format("===== Cannot call ~p/~p~n",[Fun,N])
                  end,
                 Exports).
