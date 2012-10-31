-module(chatroom_crude_test).

-compile(export_all).

test() ->
    chatroom:start_link({global,chatroom}),
    spawn_logger(),

    timer:sleep(250), print("Test: hello"),
    chatroom:global_chatroom() ! hello,

    timer:sleep(250), print("Test: leave explicitly"),
    spawn_leave_explicitly(),

    timer:sleep(250), print("Test: leave by error"),
    spawn_leave_by_error(),

    timer:sleep(1000),
    ok.


%%%

spawn_logger() ->
    Chatroom = chatroom:global_chatroom(),
    spawn(fun() ->
                  Chatroom ! {join, self()},
                  logger_loop()
          end).

logger_loop() ->
    receive M ->
            io:format("Received: ~p\n", [M])
    end,
    logger_loop().

%%%

spawn_leave_explicitly() ->
    Chatroom = chatroom:global_chatroom(),
    spawn(fun() ->
                  Chatroom ! {join, self()},
                  Chatroom ! {leave, self()}
          end).

%%%

spawn_leave_by_error() ->
    Chatroom = chatroom:global_chatroom(),
    spawn(fun() ->
                  Chatroom ! {join, self()},
                  timer:sleep(100),
                  exit("I'm going down")
          end).

%%%


print(S) ->
    io:format("~s\n", [S]).

