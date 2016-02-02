-module(test_task_resource_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([allocate/1, deallocate/1]).
%-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
	gen_server:start_link({local, test_task_resource_server}, test_task_resource_server, [], []).

allocate(Resource) ->
	gen_server:call(test_task_resource_server, allocate).

deallocate(ResourceID) ->
	gen_server:call(test_task_resource_server, deallocate).
	
list() ->
	gen_server:call(test_task_resource_server, list).
	
init(_Args) ->
	{ok, initialServerState()}.

handle_call(allocate, _From, Chs) ->
	{Ch, Chs2} = alloc(Chs),
	{reply, Ch, Chs2}.

terminate(shutdown, State) ->
	ok.