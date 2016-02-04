-module(dispatcher).

-export([dispatch_path/3]).

-include_lib("eunit/include/eunit.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module interface
dispatch_path(RequestMethod, RequestPath, QueryData) ->
	io:format("RequestPath = ~p~n", [RequestPath]),
    if length(RequestPath) == 0 ->
        bad_request();
    true ->
        io:format("path = ~p~n", [RequestPath]),
        RestFunction = hd(RequestPath),
        RestArguments = tl(RequestPath),
        {HttpStatusCode, JsonData} = dispatch_request(RestFunction, RequestMethod, RestArguments, QueryData),
        io:format("Response data : ~p~nResponse code : ~p~n", [JsonData, HttpStatusCode]),
        case JsonData of
            empty_body -> {status, HttpStatusCode};
            _ -> [{status, HttpStatusCode}, {html, JsonData}]
        end
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dispatcher itself
dispatch_request("allocate", 'GET', [Username], []) ->
    AllocResult = resource_server:allocate(Username),
    case AllocResult of
        {ok, ResourceID} -> { 201, json_encode(ResourceID) };
        error_out_of_resources -> { 503, "Out of resources" }
    end;

dispatch_request("deallocate", 'GET', [ResourceID], []) ->
    case resource_server:deallocate(list_to_atom(ResourceID)) of
        ok -> { 204, empty_body };
        error_not_found -> { 404, "Not allocated" }
    end;

dispatch_request("reset", 'GET', [], []) ->
    ok = resource_server:reset(),
    { 204, empty_body };

dispatch_request("list", 'GET', [Username], []) ->
    Result = resource_server:list(Username),
	{ 200, json_encode([state_conversion_utils:resource_to_json_id(Elem) || Elem <- Result ]) };

dispatch_request("list", 'GET', [], []) ->
    Result = resource_server:list_all(),
    { 200, json_encode({obj, state_conversion_utils:state_to_json_full(Result)}) };

dispatch_request(_BadRequest, _BadHTTPMethod, _Path, _BadQuery) ->
    bad_request().

json_encode(Object) when is_atom(Object) ->
    atom_to_list(Object) ++ "";
json_encode(Object) ->
    io:format("converting to json = ~p~n", [Object]),
    rfc4627:encode(Object).

bad_request() ->
    { 400, "Bad request" }.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testing
bad_request_test() ->
	{ok, Pid} = resource_server:start_link(),
	unlink(Pid),
	?assert( dispatch_request("list", 'PUT', "vasya", []) =:= bad_request() ),
	?assert( dispatch_request("list_error", 'GET', "vasya", []) =:= bad_request() ),
	?assert( dispatch_request("list", 'PUT', "vasya", []) =:= bad_request() ),
	?assert( dispatch_request("list_error", 'PUT', [], []) =:= bad_request() ),
	?assert( dispatch_request("list", 'PUT', [], []) =:= bad_request() ),
	?assert( dispatch_request("list_error", 'GET', [], []) =:= bad_request() ),
	?assert( dispatch_request("reset", 'POST', [], []) =:= bad_request() ),
	?assert( dispatch_request("deallocate", 'POST', [], []) =:= bad_request() ),
	?assert( dispatch_request("deallocate", 'GET', [], []) =:= bad_request() ),
	?assert( dispatch_request("allocate", 'POST', "us", []) =:= bad_request() ),
	?assert( dispatch_request("allocate", 'PUT', ["us"], []) =:= bad_request() ),
	?assert( dispatch_request("allocate", 'GET', "us", []) =:= bad_request() ),
	?assert( dispatch_request("allocate", 'GET', ["us"], "query=fail") =:= bad_request() ),
	resource_server:stop().
	
workflow_1_test() ->
	{ok, Pid} = resource_server:start_link(),
	unlink(Pid),
	?assert( dispatch_request("list", 'GET', [], []) =:= {200, json_encode( {obj, [ {allocated,[]}, {deallocated,[r1,r2,r3]} ]} )} ),
	?assert( dispatch_request("allocate", 'GET', ["ivan"], []) =:= {201, json_encode(r1)} ),
	?assert( dispatch_request("allocate", 'GET', ["dima"], []) =:= {201, json_encode(r2)} ),
	?assert( dispatch_request("allocate", 'GET', ["vasya"], []) =:= {201, json_encode(r3)} ),
	?assert( dispatch_request("allocate", 'GET', ["looser kesha"], []) =:= {503, "Out of resources"} ),
	?assert( dispatch_request("list", 'GET', ["ivan"], []) =:= {200, json_encode([r1])} ),
	?assert( dispatch_request("deallocate", 'GET', ["r1"], []) =:= {204, empty_body} ),
	?assert( dispatch_request("deallocate", 'GET', ["r2"], []) =:= {204, empty_body} ),
	?assert( dispatch_request("deallocate", 'GET', ["any"], []) =:= {404, "Not allocated"} ),
	?assert( dispatch_request("deallocate", 'GET', ["r3"], []) =:= {204, empty_body} ),
	?assert( dispatch_request("deallocate", 'GET', ["r3"], []) =:= {404, "Not allocated"} ),
	?assert( dispatch_request("deallocate", 'GET', ["all"], []) =:= {404, "Not allocated"} ),
	?assert( dispatch_request("list", 'GET', ["ivan"], []) =:= {200, json_encode([])} ),
	resource_server:stop().
	
workflow_2_test() ->
	{ok, Pid} = resource_server:start_link(),
	unlink(Pid),
	EmptyList = {obj, [ {allocated,[]}, {deallocated,[r1,r2,r3]} ]},
	?assert( dispatch_request("list", 'GET', [], []) =:= {200, json_encode( EmptyList )} ),
	?assert( dispatch_request("allocate", 'GET', ["ivan"], []) =:= {201, json_encode(r1)} ),
	?assert( dispatch_request("allocate", 'GET', ["dima"], []) =:= {201, json_encode(r2)} ),
	?assert( dispatch_request("allocate", 'GET', ["vasya"], []) =:= {201, json_encode(r3)} ),
	?assert( dispatch_request("allocate", 'GET', ["looser kesha"], []) =:= {503, "Out of resources"} ),
	?assert( dispatch_request("reset", 'GET', [], []) =:= {204, empty_body} ),
	?assert( dispatch_request("list", 'GET', ["ivan"], []) =:= {200, json_encode([])} ),
	?assert( dispatch_request("list", 'GET', [], []) =:= {200, json_encode(EmptyList)} ),
	?assert( dispatch_request("allocate", 'GET', ["ivan"], []) =:= {201, json_encode(r1)} ),
	?assert( dispatch_request("allocate", 'GET', ["dima"], []) =:= {201, json_encode(r2)} ),
	?assert( dispatch_request("allocate", 'GET', ["ivan"], []) =:= {201, json_encode(r3)} ),
	?assert( dispatch_request("list", 'GET', ["ivan"], []) =:= {200, json_encode([r1, r3])} ),
	IvanExpectedList = {obj, [ {allocated,[{obj, [{r1, <<"ivan">>}]}, {obj, [{r2, <<"dima">>}]}, {obj, [{r3, <<"ivan">>}]}]}, {deallocated,[]} ]},
	?assert( dispatch_request("list", 'GET', [], []) =:= {200, json_encode(IvanExpectedList)} ),
	?assert( dispatch_request("deallocate", 'GET', ["r1"], []) =:= {204, empty_body} ),
	IvanExpectedList2 = {obj, [ {allocated,[{obj, [{r2, <<"dima">>}]}, {obj, [{r3, <<"ivan">>}]}]}, {deallocated,[r1]} ]},
	?assert( dispatch_request("list", 'GET', [], []) =:= {200, json_encode(IvanExpectedList2)} ),
	?assert( dispatch_request("list", 'GET', ["dima"], []) =:= {200, json_encode([r2])} ),
	?assert( dispatch_request("list", 'GET', ["ivan"], []) =:= {200, json_encode([r3])} ),
	resource_server:stop().