-module(state).
-export([initial_state/0, allocate/2, deallocate/2, reset/1, list/2]).
-include_lib("eunit/include/eunit.hrl").
-include("state.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module interface
initial_state() ->
	#task_state{allocated_list=[],free_list=[ #resource{resource_id=r1}, #resource{resource_id=r2}, #resource{resource_id=r3} ]}.
	   
allocate(Username, #task_state{ allocated_list=Allocated, free_list=[FreeHead | FreeTail] } ) ->
	{ ok, FreeHead#resource.resource_id, #task_state{ allocated_list=[FreeHead#resource{username=Username}|Allocated], free_list=FreeTail } };
allocate(_, State=#task_state{ free_list=[] } ) ->
	{ error, resource_container_full, State }.

deallocate(ResourceID, #task_state{ allocated_list=AllocatedList, free_list=FreeList }) ->
	FoundAllocatedResourceList = [ Allocated || Allocated=#resource{resource_id=Res} <- AllocatedList, Res == ResourceID ],
	case FoundAllocatedResourceList of
		[_SingleInstance] ->
			ResultFreeList = [#resource{resource_id=ResourceID}|FreeList],
			AllocatedResourceList = [ Allocated || Allocated=#resource{resource_id=Res} <- AllocatedList, Res /= ResourceID ],
			{ ok, #task_state{ allocated_list=AllocatedResourceList, free_list=ResultFreeList } };
		_ ->
			{ error, resource_not_found }
	end;
deallocate(_, _) ->
	{ error, unknown }.
	
reset(_) ->
	{ok, initial_state()}.

list([], State) ->
	{ok, State};
list(Username, State) ->
	FoundResources = [ Found || Found <- State#task_state.allocated_list, Found#resource.username == Username ],
	{ok, FoundResources}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testing
ok_result( { ok, State } ) ->
	State.

allocate_multiple(0, State) ->
	{ok, State};
allocate_multiple(N, State) ->
	{ Status, FailReason, NewState } = allocate("Username" ++ hd(io_lib:format("~p", [N])), State),
	RecursionResult = allocate_multiple(N-1, NewState),
	case Status of
		ok -> RecursionResult;
		error -> { error, FailReason }
	end.

deallocate_multiple(0, State) ->
	{ok, State};
deallocate_multiple(N, State) ->
	StateComplexNew = { StatusNew, StateNew } = deallocate(list_to_atom( "r" ++ hd(io_lib:format("~p", [N]))), State),
	case StatusNew of
		error -> StateComplexNew;
		_ -> deallocate_multiple(N-1, StateNew)
	end.

list_test() ->
	State = ok_result(allocate_multiple(3, initial_state())),
	?assert( list("Username1", State) =:= {ok, [#resource{username="Username1", resource_id=r3} ]} ),
	?assert( list("Username2", State) =:= {ok, [#resource{username="Username2", resource_id=r2} ]} ),
	?assert( list("Username3", State) =:= {ok, [#resource{username="Username3", resource_id=r1} ]} ),
	?assert( list("Username4", State) =:= {ok, []} ),
	?assert( list([], State) =:= {ok, State}).

reset_test_() ->
	[?_assert( reset(ok_result(allocate_multiple(3, initial_state()))) =:= {ok, initial_state()}),
	 ?_assert( reset(ok_result(allocate_multiple(1, initial_state()))) =:= {ok, initial_state()})
	].
	
allocate_bad_data_test_() ->
	[?_assert( allocate_multiple(4, initial_state()) =:= {error, resource_container_full})
	].
	
deallocate_bad_data_test_() ->
	[?_assert( deallocate_multiple(1, initial_state()) =:= {error, resource_not_found}),
	 ?_assert( deallocate_multiple(4, ok_result(allocate_multiple(3, initial_state()))) =:= {error, resource_not_found}),
	 ?_assert( deallocate_multiple(4, ok_result(allocate_multiple(1, initial_state()))) =:= {error, resource_not_found})
	].
	
allocate_deallocate_preserve_initial_state_test_() ->
	[?_assert( deallocate_multiple(1, ok_result(allocate_multiple(1, initial_state()))) =:= {ok, initial_state()}),
	 ?_assert( deallocate_multiple(2, ok_result(allocate_multiple(2, initial_state()))) =:= {ok, initial_state()}),
	 ?_assert( deallocate_multiple(3, ok_result(allocate_multiple(3, initial_state()))) =:= {ok, initial_state()})
	].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
