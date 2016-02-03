-module(state_conversion_utils).

-export([resource_to_json_id/1, resource_to_json_full/1, state_to_json_full/1]).

-include("state.hrl").

{obj, [{"a", 1}, {"b", 2}]}

state_to_json_full(#task_state{allocated_list=AllocatedList, free_list=FreeList}) ->
	[{ "allocated", [resource_to_json_full(Elem) || Elem <- AllocatedList]},
	 { "deallocated", [resource_to_json_id(Elem) || Elem <- FreeList]}].

resource_to_json_id(#resource{resource_id=ResourceId}) ->
	atom_to_list(ResourceId).

resource_to_json_full(#resource{resource_id=ResourceId, username=Username}) ->
	{ atom_to_list(ResourceId), Username }.