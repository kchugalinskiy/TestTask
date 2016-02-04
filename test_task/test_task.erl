-module(test_task).
-export([out/1, start/0]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/yaws_api.hrl").
-include("state.hrl").

start() ->
	resource_server_sup:start_link().

out(A) ->
    Request = A#arg.req,
    RequestMethod = Request#http_request.method,
    {_, RelPath} = Request#http_request.path,
    RequestPath = string:tokens(RelPath, "/"),
    io:format("RequestPath = ~p~n", [RequestPath]),
    if length(RequestPath) == 0 ->
        bad_request();
    true ->
        io:format("path = ~p~n", [RequestPath]),
        RestFunction = hd(RequestPath),
        RestArguments = tl(RequestPath),
        {HttpStatusCode, JsonData} = dispatch_request(RestFunction, RequestMethod, RestArguments),
        io:format("Response data : ~p~nResponse code : ~p~n", [JsonData, HttpStatusCode]),
        case JsonData of
            empty_body -> {status, HttpStatusCode};
            _ -> [{status, HttpStatusCode}, {html, JsonData}]
        end
    end.

dispatch_request("allocate", 'GET', [Username]) ->
    AllocResult = resource_server:allocate(Username),
    case AllocResult of
        {ok, ResourceID} -> { 201, atom_to_list(ResourceID) ++ "" };
        error_out_of_resources -> { 503, "Out of resources" }
    end;

dispatch_request("deallocate", 'GET', [ResourceID]) ->
    case resource_server:deallocate(ResourceID) of
        ok -> { 204, empty_body };
        error_not_found -> { 404, "Not allocated" }
    end;

dispatch_request("reset", 'GET', []) ->
    ok = resource_server:reset(),
    { 204, empty_body };

dispatch_request("list", 'GET', Path) ->
    Result = resource_server:list(Path),
    case Path of
        Path when is_list(Path) and length(Path) > 2 -> bad_request();
        [] -> { 200, json_encode({obj, state_conversion_utils:state_to_json_full(Result)}) };
        _  -> { 200, json_encode([state_conversion_utils:resource_to_json_id(Elem) || Elem <- Result ]) }
    end;

dispatch_request(_BadRequest, _BadHTTPMethod, _Path) ->
    bad_request().

json_encode(Object) ->
    io:format("converting to json = ~p~n", [Object]),
    rfc4627:encode(Object).

bad_request() ->
    { 400, "Bad request" }.