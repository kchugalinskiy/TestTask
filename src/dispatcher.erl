-module(dispatcher).

-export([dispatch_path/3]).

-include_lib("eunit/include/eunit.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module interface
dispatch_path(RequestMethod, RequestPath, QueryData) ->
    if length(RequestPath) == 0 ->
        bad_request();
    true ->
        RestFunction = hd(RequestPath),
        RestArguments = tl(RequestPath),
		%io:format("request function = ~p~nMethod = ~p~nRestArguments = ~p~nData = ~p~n", [RestFunction, RequestMethod, RestArguments, QueryData]),
        {HttpStatusCode, JsonData} = dispatch_request(RestFunction, RequestMethod, RestArguments, QueryData),
        case JsonData of
            empty_body -> {status, HttpStatusCode};
            _ -> [{status, HttpStatusCode}, {html, JsonData}]
        end
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dispatcher itself
dispatch_request("allocate", 'GET', [Username], undefined) ->
    AllocResult = resource_server:allocate(Username),
    case AllocResult of
        {ok, ResourceID} -> { 201, json_encode(ResourceID) };
        error_out_of_resources -> { 503, "Out of resources" }
    end;

dispatch_request("deallocate", 'GET', [ResourceID], undefined) ->
    case resource_server:deallocate(list_to_atom(ResourceID)) of
        ok -> { 204, empty_body };
        error_not_found -> { 404, "Not allocated" }
    end;

dispatch_request("reset", 'GET', [], undefined) ->
    ok = resource_server:reset(),
    { 204, empty_body };

dispatch_request("list", 'GET', [Username], undefined) ->
    Result = resource_server:list(Username),
	{ 200, json_encode([state_conversion_utils:resource_to_json_id(Elem) || Elem <- Result ]) };

dispatch_request("list", 'GET', [], undefined) ->
    Result = resource_server:list_all(),
    { 200, json_encode({obj, state_conversion_utils:state_to_json_full(Result)}) };

dispatch_request(BadRequest, BadHTTPMethod, Path, BadQuery) ->
	io:format("Bad request = ~p~nHTTP method = ~p~nPath = ~p~nQuery = ~p~n", [BadRequest, BadHTTPMethod, Path, BadQuery]),
    bad_request().

json_encode(Object) when is_atom(Object) ->
    atom_to_list(Object) ++ "";
json_encode(Object) ->
    rfc4627:encode(Object).

bad_request() ->
    { 400, "Bad request" }.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testing
bad_request_test() ->
	{ok, Pid} = resource_server:start_link(),
	unlink(Pid),
	?assert( dispatch_request("list", 'PUT', "vasya", undefined) =:= bad_request() ),
	?assert( dispatch_request("list_error", 'GET', "vasya", undefined) =:= bad_request() ),
	?assert( dispatch_request("list", 'PUT', "vasya", undefined) =:= bad_request() ),
	?assert( dispatch_request("list_error", 'PUT', [], undefined) =:= bad_request() ),
	?assert( dispatch_request("list", 'PUT', [], undefined) =:= bad_request() ),
	?assert( dispatch_request("list_error", 'GET', [], undefined) =:= bad_request() ),
	?assert( dispatch_request("reset", 'POST', [], undefined) =:= bad_request() ),
	?assert( dispatch_request("deallocate", 'POST', [], undefined) =:= bad_request() ),
	?assert( dispatch_request("deallocate", 'GET', [], undefined) =:= bad_request() ),
	?assert( dispatch_request("allocate", 'POST', "us", undefined) =:= bad_request() ),
	?assert( dispatch_request("allocate", 'PUT', ["us"], undefined) =:= bad_request() ),
	?assert( dispatch_request("allocate", 'GET', "us", undefined) =:= bad_request() ),
	?assert( dispatch_request("allocate", 'GET', ["us"], "query=fail") =:= bad_request() ),
	resource_server:stop().
	
workflow_1_test() ->
	{ok, Pid} = resource_server:start_link(),
	unlink(Pid),
	?assert( dispatch_request("list", 'GET', [], undefined) =:= {200, json_encode( {obj, [ {allocated,[]}, {deallocated,[r1,r2,r3]} ]} )} ),
	?assert( dispatch_request("allocate", 'GET', ["ivan"], undefined) =:= {201, json_encode(r1)} ),
	?assert( dispatch_request("allocate", 'GET', ["dima"], undefined) =:= {201, json_encode(r2)} ),
	?assert( dispatch_request("allocate", 'GET', ["vasya"], undefined) =:= {201, json_encode(r3)} ),
	?assert( dispatch_request("allocate", 'GET', ["looser kesha"], undefined) =:= {503, "Out of resources"} ),
	?assert( dispatch_request("list", 'GET', ["ivan"], undefined) =:= {200, json_encode([r1])} ),
	?assert( dispatch_request("deallocate", 'GET', ["r1"], undefined) =:= {204, empty_body} ),
	?assert( dispatch_request("deallocate", 'GET', ["r2"], undefined) =:= {204, empty_body} ),
	?assert( dispatch_request("deallocate", 'GET', ["any"], undefined) =:= {404, "Not allocated"} ),
	?assert( dispatch_request("deallocate", 'GET', ["r3"], undefined) =:= {204, empty_body} ),
	?assert( dispatch_request("deallocate", 'GET', ["r3"], undefined) =:= {404, "Not allocated"} ),
	?assert( dispatch_request("deallocate", 'GET', ["all"], undefined) =:= {404, "Not allocated"} ),
	?assert( dispatch_request("list", 'GET', ["ivan"], undefined) =:= {200, json_encode([])} ),
	resource_server:stop().
	
workflow_2_test() ->
	{ok, Pid} = resource_server:start_link(),
	unlink(Pid),
	EmptyList = {obj, [ {allocated,[]}, {deallocated,[r1,r2,r3]} ]},
	?assert( dispatch_request("list", 'GET', [], undefined) =:= {200, json_encode( EmptyList )} ),
	?assert( dispatch_request("allocate", 'GET', ["ivan"], undefined) =:= {201, json_encode(r1)} ),
	?assert( dispatch_request("allocate", 'GET', ["dima"], undefined) =:= {201, json_encode(r2)} ),
	?assert( dispatch_request("allocate", 'GET', ["vasya"], undefined) =:= {201, json_encode(r3)} ),
	?assert( dispatch_request("allocate", 'GET', ["looser kesha"], undefined) =:= {503, "Out of resources"} ),
	?assert( dispatch_request("reset", 'GET', [], undefined) =:= {204, empty_body} ),
	?assert( dispatch_request("list", 'GET', ["ivan"], undefined) =:= {200, json_encode([])} ),
	?assert( dispatch_request("list", 'GET', [], undefined) =:= {200, json_encode(EmptyList)} ),
	?assert( dispatch_request("allocate", 'GET', ["ivan"], undefined) =:= {201, json_encode(r1)} ),
	?assert( dispatch_request("allocate", 'GET', ["dima"], undefined) =:= {201, json_encode(r2)} ),
	?assert( dispatch_request("allocate", 'GET', ["ivan"], undefined) =:= {201, json_encode(r3)} ),
	?assert( dispatch_request("list", 'GET', ["ivan"], undefined) =:= {200, json_encode([r1, r3])} ),
	IvanExpectedList = {obj, [ {allocated,[{obj, [{r1, <<"ivan">>}]}, {obj, [{r2, <<"dima">>}]}, {obj, [{r3, <<"ivan">>}]}]}, {deallocated,[]} ]},
	?assert( dispatch_request("list", 'GET', [], undefined) =:= {200, json_encode(IvanExpectedList)} ),
	?assert( dispatch_request("deallocate", 'GET', ["r1"], undefined) =:= {204, empty_body} ),
	IvanExpectedList2 = {obj, [ {allocated,[{obj, [{r2, <<"dima">>}]}, {obj, [{r3, <<"ivan">>}]}]}, {deallocated,[r1]} ]},
	?assert( dispatch_request("list", 'GET', [], undefined) =:= {200, json_encode(IvanExpectedList2)} ),
	?assert( dispatch_request("list", 'GET', ["dima"], undefined) =:= {200, json_encode([r2])} ),
	?assert( dispatch_request("list", 'GET', ["ivan"], undefined) =:= {200, json_encode([r3])} ),
	resource_server:stop().