-module(chatroom).

%% Public API:
-export([start_link/1]).

%% For spawning:
-export([init/1]).

-record(state, {subscribers}).

start_link(Name) ->
    proc_lib:start_link(?MODULE, init, [Name]).

init(Name) ->
    case Name of
        [] ->
            ok;
        {global, GName} ->
            yes = global:register_name(GName, self());
        {local,Name} ->
            erlang:register(Name, self())
    end,
    proc_lib:init_ack(self()),
    loop(#state{subscribers=[]}).

loop(State) ->
    receive
        {subscribe, Pid} when is_pid(Pid) ->
            loop(add_subscriber(State, Pid));
        {unsubscribe, Pid} when is_pid(Pid) ->
            loop(remove_subscriber(State, Pid));
        Msg ->
            publish(State, Msg),
            loop(State)
    end.

add_subscriber(State=#state{subscribers=Subs}, Pid) ->
    %% TODO: monitor!
    State2 = State#state{subscribers=[Pid|Subs]},
    error_logger:info_msg("Joined: ~p\n", [Pid]),
    publish(State2, {joined_chatroom, Pid}),
    State2.

remove_subscriber(State=#state{subscribers=Subs}, Pid) ->
    State2 = State#state{subscribers=lists:delete(Pid,Subs)},
    error_logger:info_msg("Left: ~p\n", [Pid]),
    publish(State2, {left_chatroom, Pid}),
    State2.


publish(#state{subscribers=Subs}, Msg) ->
    lists:foreach(fun(Subscriber) ->
                          Subscriber ! Msg
                  end,
                  Subs).
