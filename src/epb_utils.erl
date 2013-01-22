-module(epb_utils).
-compile([export_all]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Find new processes %%%%%%

start_snapshotter() ->
    spawn(?MODULE, init_snapshotter, []).

call(Pid, Msg) ->
    Pid ! Msg,
    receive
	{ok, Res} ->
	    Res;
	ok ->
	    ok
    after
	100 ->
	    timeout
    end.

diff() ->
    call(snapshotter, {diff, self()}).

snapshot() ->
    call(snapshotter, {snapshot, self()}).

init_snapshotter() ->
    process_flag(trap_exit, true),
    true = erlang:register(snapshotter, self()),
    loop_snapshotter(processes()).

loop_snapshotter(Processen) ->
    receive
	{diff, Pid} ->
	    Diff = processes() -- Processen,
	    Pid ! {ok, [process_details(P) || P <- Diff]},
	    ?MODULE:loop_snapshotter(Processen);
	{snapshot, Pid} ->
	    Pid ! ok,
	    ?MODULE:loop_snapshotter(processes());
	Msg ->
	    io:format("Not understood: ~p~n", [Msg]),
	    ?MODULE:loop_snapshotter(Processen)
    after
	300 ->
	    ?MODULE:loop_snapshotter(Processen)
    end.

process_details(Process) ->
    InitCall = case process_info(Process, dictionary) of
		   undefined ->
		       unknown;
		   {dictionary, Props} ->
		       lists:keyfind('$initial_call', 1, Props)
	       end,
    {Process,
     process_info(Process, registered_name),
     process_info(Process, current_function),
     process_info(Process, initial_call),
     InitCall}.
