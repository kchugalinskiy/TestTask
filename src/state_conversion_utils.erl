-module(state_conversion_utils).

-export([resource_to_json_id/1, resource_to_json_full/1, state_to_json_full/1]).
-include_lib("eunit/include/eunit.hrl").
-include("state.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface
state_to_json_full(#task_state{allocated_list=AllocatedList, free_list=FreeList}) ->
	[{ "allocated", [resource_to_json_full(Elem) || Elem <- AllocatedList]},
	 { "deallocated", [resource_to_json_id(Elem) || Elem <- FreeList]}].

resource_to_json_id(#resource{resource_id=ResourceId}) ->
	ResourceId.

resource_to_json_full(#resource{resource_id=ResourceId, username=Username}) ->
	{obj, [{ResourceId, list_to_binary(Username)}] }.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testing
resource_conversion_test_() ->
	[?_assert( resource_to_json_id(#resource{resource_id=r1}) =:= r1 ),
	 ?_assert( resource_to_json_full(#resource{resource_id=r1, username="Username"}) =:= {obj, [{r1, <<"Username">>}]} )
	].