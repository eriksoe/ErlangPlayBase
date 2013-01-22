-module(alarm_bot).

%% Synopsis:
%% alarm_bot:start_link(chatroom:local_chatroom()).

%% Public API
-export([start_link/1]).

%% For spawning
-export([init/1]).

-define(COMMAND, "!alarm ").
-define(SAY_PREFIX, "alarm_bot: ").

start_link(ChatRoom) when is_pid(ChatRoom) ->
    proc_lib:start_link(?MODULE, init, [ChatRoom]).

init(ChatRoom) ->
    proc_lib:init_ack(self()),
    ChatRoom ! {join, self()},
    loop(ChatRoom).

loop(ChatRoom) ->
    receive
        ?COMMAND ++ Msg ->
            case re:run(Msg, "^\\s*(\\d+)(ms|s|m|h)\s+(.*)", [{capture,all_but_first,list}]) of
                nomatch ->
                    Usage = "Usage: "++?COMMAND++" <amount>(ms|s|m|h) <message>",
                    ChatRoom ! Usage;
                {match, [AmountStr, TimeUnit, AlarmMsg]} ->
                    Millis =
                        list_to_integer(AmountStr) * timeunit_in_ms(TimeUnit),
                    timer:send_after(Millis, ChatRoom, ?SAY_PREFIX++AlarmMsg),
                    ChatRoom ! ?SAY_PREFIX++"Started a timer."
            end;
        _ ->
            ok
    end,
    ?MODULE:loop(ChatRoom).

timeunit_in_ms("ms") -> ms;
timeunit_in_ms("s") -> 1000;
timeunit_in_ms("m") -> 1000*60;
timeunit_in_ms("h") -> 1000*60*60.
