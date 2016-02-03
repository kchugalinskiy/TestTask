-module(resource_server).
-export([allocate/1, deallocate/1, list/1, reset/0]).
-include_lib("eunit/include/eunit.hrl").
-include("state.hrl").

-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, terminate/2]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	gen_server:start_link( { global, resource_server }, resource_server, [], [] ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
allocate(Username) ->
	gen_server:call({global, resource_server}, { allocate, Username } ).

deallocate(ResourceID) ->
	gen_server:call({global, resource_server}, { deallocate, ResourceID } ).

list(Username) ->
	gen_server:call({global, resource_server}, { list, Username } ).

reset() ->
	gen_server:call({global, resource_server}, reset).

stop() ->
    gen_server:call({global, resource_server}, stop).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
	{ok, state:initial_state()}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call( { allocate, Username }, _From, State) ->
	case state:allocate(Username, State) of
		{ ok, NewState } -> {reply, ok, NewState};
		{ error, _ } -> {reply, error_out_of_resources, State}
	end;

handle_call( { deallocate, ResourceID }, _From, State) ->
	case state:deallocate(ResourceID, State) of
		{ ok, NewState } -> {reply, ok, NewState};
		{ error, _ } -> {reply, error_not_found, State}
	end;

handle_call( { list, Username }, _From, State) ->
	{ok, Reply} = state:list(Username, State),
	{reply, Reply, State};

handle_call( reset, _From, State) ->
	{ok, StateModified} = state:reset(State),
	{reply, ok, StateModified};

handle_call( stop, _, State ) ->
    {stop, shutdown, ok, State};

handle_call( _, _, State) ->
	io:format("handle_call nothing~n", []),
	{noreply, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terminate(shutdown, _State) ->
	ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testing

start_stop_test() ->
	{ok, Pid} = start_link(),
	unlink(Pid),
	?assert( stop() =:= ok).

allocate_overrun_test() ->
	{ok, Pid} = start_link(),
	unlink(Pid),
	?assert( allocate("ivan") =:= ok ),
	?assert( allocate("dima") =:= ok ),
	?assert( allocate("andrey") =:= ok ),
	?assert( allocate("slava") =:= error_out_of_resources ),
	stop().

allocate_overrun_dealloc_test() ->
	{ok, Pid} = start_link(),
	unlink(Pid),
	?assert( allocate("ivan") =:= ok ),
	?assert( allocate("dima") =:= ok ),
	?assert( allocate("andrey") =:= ok ),
	?assert( allocate("slava") =:= error_out_of_resources ),
	?assert( deallocate(r1) =:= ok ),
	?assert( deallocate(r1) =:= error_not_found ),
	?assert( deallocate(r2) =:= ok ),
	?assert( deallocate(r3) =:= ok ),
	?assert( deallocate(r1) =:= error_not_found ),
	?assert( deallocate(r2) =:= error_not_found ),
	?assert( deallocate(r3) =:= error_not_found ),
	stop().

list_reset_test() ->
	{ok, Pid} = start_link(),
	unlink(Pid),
	?assert( allocate("ivan") =:= ok ),
	?assert( allocate("dima") =:= ok ),
	?assert( allocate("ivan") =:= ok ),
	?assert( allocate("slava") =:= error_out_of_resources ),
	ReferenceState = #task_state{
		allocated_list=[#resource{resource_id=r3, username="ivan"},
						#resource{resource_id=r2, username="dima"},
						#resource{resource_id=r1, username="ivan"}],
		free_list=[]
	},
	ReferenceListDuplicate = [ #resource{resource_id=r3, username="ivan"}, #resource{resource_id=r1, username="ivan"} ],
	?assert( list([]) == ReferenceState ),
	?assert( list("ivan") == ReferenceListDuplicate ),
	?assert( reset() =:= ok ),
	?assert( list([]) == state:initial_state() ),
	?assert( list("ivan") == [] ),
	?assert( allocate("ivan") =:= ok ),
	?assert( allocate("dima") =:= ok ),
	?assert( allocate("andrey") =:= ok ),
	?assert( allocate("slava") =:= error_out_of_resources ),
	ReferenceState2 = #task_state{
		allocated_list=[#resource{resource_id=r3, username="andrey"},
						#resource{resource_id=r2, username="dima"},
						#resource{resource_id=r1, username="ivan"}],
		free_list=[]
	},
	?assert( list([]) == ReferenceState2 ),
	?assert( list("dima") == [#resource{resource_id=r2, username="dima"}] ),
	stop().