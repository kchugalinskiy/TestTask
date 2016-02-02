-module(test_task_state).
-record(task_state, {r1, r2, r3}).

initialServerState() ->
	
	   
alloc(Arg) ->
	Arg.

free(Channel, Arg) ->
	{ Channel, Arg }.