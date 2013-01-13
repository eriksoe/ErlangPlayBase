-module(chatroom_sup).
-export([start_link/0, init/0, loop/3]).

-define(FULL_BURST_TANK, 2).
-define(REFILL_RATE, 10000).
-define(ROOM_NAME, common_room).

start_link() ->
    proc_lib:start_link(?MODULE, init, []).

bot_specs(Room) ->
    [{echo_bot, start_link, Room},
     {alarm_bot, start_link, Room}].

init() ->
    erlang:process_flag(trap_exit, true),
    %% TODO global registration
    true = erlang:register(?MODULE, self()),
    proc_lib:init_ack(self()),
    start_the_common_room(?FULL_BURST_TANK).

start_the_common_room(BurstTank) ->
    Room = chatroom:start_link({local, ?ROOM_NAME}),
    ?MODULE:loop(BurstTank, Room, restart_bots(bot_specs(Room))).

restart_bots(BotSpecs) ->
    [maybe_start_bot(BotSpec) || BotSpec <- BotSpecs].

maybe_start_bot({M,F,A}) ->
    io:format("Starting bot: ~p, ~p, ~p~n", [M,F,A]),
    case M:F(A) of
	{ok, Pid} ->
	    {Pid, M,F,A};
	Pid when is_pid(Pid) -> 
	    {Pid, M,F,A}
    end;
maybe_start_bot({Pid, M,F,A} = Bot) when is_pid(Pid) ->
    case is_process_alive(Pid) of
	true ->
	    Bot;
	false ->
	    maybe_start_bot({M,F,A})
    end.

loop(BurstTank, Room, Bots) ->
    receive
	{'EXIT', Pid, Reason} ->
	    case empty(BurstTank) of
		yes ->
		    io:format("Recieved EXIT (~p, ~p), but out bursts~n", [Pid, Reason]),
		    shutdown_everything(Room, Bots);
		no ->
		    case {known_bot(Pid, Bots), Pid} of
			{{yes, M,F,A},_} ->
			    io:format("Bot crashed, ~p:~p(~p), with reason: ~p.  Restarting bot~n",
				      [M,F,A, Reason]),
			    ?MODULE:loop(fire_burst(BurstTank), Room, restart_bots(Bots));
			{no, Room} ->
			    io:format("The common room crashed with reason: ~p.  Killing Bots and restarting~n",
				      [Reason]),
			    kill_bots(Bots),
			    start_the_common_room(fire_burst(BurstTank));
			{no, _UnknownPid} ->
			    io:format("Exit from unknown process: ~p, with reason: ~p~n", [Pid, Reason]),
			    shutdown_everything(Room, Bots)
		    end
	    end;
	refill_burst ->
	    BurstTank2 = refill_burst(BurstTank),
	    io:format("Burst refill recieved ~p~n", [BurstTank2]),
	    ?MODULE:loop(BurstTank2, Room, Bots);
	Msg ->
	    io:format("Unknown msg: ~p~n",[Msg]),
	    shutdown_everything(Room, Bots)
    end.

empty(N) when N =< 0 ->
    yes;
empty(_) ->
    no.

fire_burst(N) when N =< 0 ->
    out_of_bursts;
fire_burst(N) ->
    call_for_a_refill(),
    N-1.

refill_burst(N) ->
    case N+1 of
	?FULL_BURST_TANK ->
	    ?FULL_BURST_TANK;
	N2 ->
	    call_for_a_refill(),
	    N2
    end.

call_for_a_refill() ->
    erlang:send_after(?REFILL_RATE, self(), refill_burst).

known_bot(Pid, Bots) ->
    case [Spec || {BotPid, _, _, _} = Spec <- Bots, Pid == BotPid] of
	[] ->
	    no;
	[{Pid, M,F,A}] ->
	    {yes, M,F,A}
    end.

shutdown_everything(Room, Bots) ->
    io:format("shutting down everything~n"),
    kill_bots(Bots),
    exit(Room, shutdown),
    exit(shutdown).

kill_bots(Bots) ->
    [begin unlink(BotPid), exit(BotPid, kill) end || {BotPid, _,_,_} <- Bots].
