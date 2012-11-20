-module(chatroom_sup).
-compile([export_all]).

talk(Msg) ->
    common_room ! Msg.

start() ->
    Sup = spawn(?MODULE, init, [common_room, self()]),
    receive
	{started, Sup} ->
	    {ok, Sup}
    after
	100 ->
	    exit(Sup, kill),
	    {error, timeout}
    end.

stop() ->
    ?MODULE ! shutdown.

init(_Name, Parent) ->
    erlang:process_flag(trap_exit, true),
    true = erlang:register(?MODULE, self()),
    Parent ! {started, self()},
    init_common_room().

init_common_room() ->
    Room = chatroom:start_link({local, common_room}),
    init_echo(Room).

init_echo(Room) ->
    Echo = echo(),
    ?MODULE:loop({Room, Echo}).

loop({Room, Echo}) ->
    receive
	shutdown ->
	    exit(Room, shutdown),
	    exit(Echo, shutdown);
	{'EXIT', Echo, Reason} ->
	    io:format("Echo crashed with reason: ~p.  Restarting Echo~n", [Reason]),
	    ?MODULE:init_echo(Room);
	{'EXIT', Room, Reason} ->
	    io:format("The common room crashed with reason: ~p.  Killing Echo and restarting~n", [Reason]),
	    unlink(Echo),
	    exit(Echo, kill),
	    ?MODULE:init_common_room();
	{'EXIT', Pid, Reason} ->
	    io:format("Exit from unknown process: ~p, with reason: ~p~n", [Pid, Reason]),
	    ?MODULE:loop({Room, Echo});
	Msg ->
	    io:format("Unknown msg: ~p~n",[Msg]),
	    ?MODULE:loop({Room, Echo})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo() ->
    spawn_link(?MODULE, echo2, [common_room]).

echo2(Room) ->
    Room ! {join, self()},
    echo3(Room).

echo3(Room) ->
    receive
	Msg ->
	    io:format("=== ECHO: ~p~n", [Msg]),
	    ?MODULE:echo3(Room)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
     InitCall}.
