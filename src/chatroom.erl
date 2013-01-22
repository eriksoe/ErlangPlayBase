-module(chatroom).

%% Public API:
-export([start_link/1,
         local_chatroom/0,
         global_chatroom/0]).

%% For spawning:
-export([init/1]).

-record(subscriber, {
          pid :: pid()
}).
-record(state, {
          subscribers :: #subscriber{}
}).

%%%========== API: ==============================

start_link(Name) ->
    proc_lib:start_link(?MODULE, init, [Name]).

local_chatroom() ->
    whereis(chatroom).

global_chatroom() ->
    global:whereis_name(chatroom).


%%%========== Internal functions: ==============================

init(Name) ->
    process_flag(trap_exit, true),
    case Name of
        [] ->
            ok;
        {global, GName} ->
            yes = global:register_name(GName, self());
        {local,LName} ->
            erlang:register(LName, self())
    end,
    proc_lib:init_ack(self()),
    loop(#state{subscribers=[]}).

loop(State) ->
    receive
        {join, Pid} when is_pid(Pid) ->
            loop(add_subscriber(State, Pid));
        {leave, Pid} when is_pid(Pid) ->
            loop(remove_subscriber(State, Pid, left));
        {'EXIT', Pid, ExitReason} ->
            loop(remove_subscriber(State, Pid, ExitReason));
        Msg ->
            publish(State, Msg),
            loop(State)
    end.

add_subscriber(State=#state{subscribers=Subs}, Pid) ->
    link(Pid), % So that the bot will die when the room disappears
    NewSubscriber = #subscriber{pid=Pid},
    State2 = State#state{subscribers=[NewSubscriber|Subs]},

    error_logger:info_msg("Joined: ~p\n", [Pid]),
    publish(State2, {joined_chatroom, Pid}),
    State2.

remove_subscriber(State=#state{subscribers=Subs}, Pid, Reason) ->
    State2 = State#state{subscribers=lists:keydelete(Pid,#subscriber.pid,Subs)},
    error_logger:info_msg("Left: ~p (reason: ~p)\n", [Pid, Reason]),
    publish(State2, {left_chatroom, Pid, Reason}),
    State2.


publish(#state{subscribers=Subs}, Msg) ->
    lists:foreach(fun(#subscriber{pid=Pid}) -> Pid ! Msg end,
                  Subs).
