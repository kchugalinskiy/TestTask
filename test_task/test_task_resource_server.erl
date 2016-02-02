-module(test_task_resource_server).
-export([allocate/1, deallocate/1, list/0, reset/0]).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, terminate/2]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	gen_server:start_link( { local, test_task_resource_server }, test_task_resource_server, [], [] ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
allocate(Resource) ->
	gen_server:call(test_task_resource_server, { allocate, Resource } ).

deallocate(ResourceID) ->
	gen_server:call(test_task_resource_server, { deallocate, ResourceID } ).
	
list() ->
	gen_server:call(test_task_resource_server, list).
	
reset() ->
	gen_server:call(test_task_resource_server, reset).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
	{ok, test_task_state:initial_state()}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call( { allocate, Resource }, _From, State) ->
	StateModified = test_task_state:allocate(Resource, State),
	Reply = State,
	{reply, Reply, StateModified};

handle_call( { deallocate, ResourceID }, _From, State) ->
	StateModified = test_task_state:deallocate(ResourceID, State),
	Reply = State,
	{reply, Reply, StateModified};

handle_call( list, _From, State) ->
	StateModified = State,
	Reply = State,
	{reply, Reply, StateModified};

handle_call( reset, _From, State) ->
	StateModified = test_task_state:reset(State),
	Reply = State,
	{reply, Reply, StateModified};

handle_call( _, _, State) ->
	io:format("handle_call nothing~n", []),
	{noreply, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terminate(shutdown, _State) ->
	ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%