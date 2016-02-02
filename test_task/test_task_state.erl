-module(test_task_state).
-export([initial_state/0]).

-record(task_state, {r1, r2, r3}).

initial_state() ->
	#task_state{r1=undefined,r2=undefined,r3=undefined}.
	   
allocate(Resource, State) ->
	State.

deallocate(ResourceID, State) ->
	State.

reset(State) ->
	initial_state().
%alloc(Arg) ->
%	Arg.

%free(Channel, Arg) ->
%	{ Channel, Arg }.