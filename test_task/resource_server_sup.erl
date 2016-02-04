-module(resource_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include_lib("eunit/include/eunit.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module interface
start_link() ->
	   supervisor:start_link(resource_server_sup, []).

init(_Args) ->
   {ok, {{one_for_one, 1, 60},
		 [
			{resource_server, {resource_server, start_link, []}, permanent, brutal_kill, worker, [resource_server]}
		 ]}}. 
		 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testing
start_shutdown_test() ->
	{ok, Pid} = start_link(),
	unlink(Pid),
	exit(Pid, shutdown).