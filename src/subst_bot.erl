-module(subst_bot).

%% Synopsis:
%% subst_bot:start_link(chatroom:local_chatroom()).

%% Public API
-export([start_link/1]).

%% For spawning
-export([init/1]).

-define(COMMAND, "!subst ").
-define(SAY_PREFIX, "subst_bot: ").

-record(state, {active=false, from, to}).

start_link(ChatRoom) when is_pid(ChatRoom) ->
    proc_lib:start_link(?MODULE, init, [ChatRoom]).

init(ChatRoom) ->
    proc_lib:init_ack(self()),
    ChatRoom ! {join, self()},
    loop(ChatRoom, #state{}).

loop(ChatRoom, State) ->
    receive
        ?COMMAND ++ "disable" ->
            ChatRoom ! ?SAY_PREFIX++"Disabled.",
            State2 = State#state{active=false};
        ?COMMAND ++ Msg ->
            case re:run(Msg, "^\\s*(\\S+)\\s+(\\S+)\\s*$", [{capture,all_but_first,list}]) of
                nomatch ->
                    Usage = "Usage: "++?COMMAND++" <from> <to>",
                    ChatRoom ! Usage,
                    State2 = State;
                {match, [From,To]} ->
                    ChatRoom ! ?SAY_PREFIX++"Set substitution ("++From++" -> "++To++")",
                    State2 = State#state{from=From, to=To, active=true}
            end;
        Msg when State#state.active ->
            try
                #state{from=From, to=To} = State,
                Substed = re:replace(Msg, From, To, [global, {return,list}]),
                (Substed == Msg) orelse ChatRoom ! Substed % Send if different
            catch _:_ -> % Not a string, or regexp error
                    ok
            end,
            State2 = State;
        _ -> % no substitution defined.
            State2 = State
    end,
    loop(ChatRoom, State2).
