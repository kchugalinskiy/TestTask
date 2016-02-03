-module(test_task).
-export([out/1, start/0]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/yaws_api.hrl").
	
start() ->
	resource_server:start_link().

out(A) ->
    Request = A#arg.req,
    RequestMethod = Request#http_request.method,
	RequestPath = tokens( Request#http_request.path, ["/"],
    dispatch_request(A#arg.pathinfo, RequestMethod, []),
    JsonData = rfc4627:encode( {obj, [{"a", 1}, {"b", 2}]} ),
    io:format("Json data : ~p~n", [JsonData]),
    { html, JsonData }.

dispatch_request("allocate", 'GET', [Username]) ->
  { ok, resource_server:allocate(Username) };

dispatch_request("deallocate", 'GET', [ResourceID]) ->
  { ok, resource_server:deallocate(ResourceID) };

dispatch_request("reset", 'GET', []) ->
  { ok, resource_server:reset() };

dispatch_request("list", 'GET', []) ->
  { ok, resource_server:list() };

dispatch_request(BadRequest, BadHTTPMethod, Params) ->
  { error, { bad_request } }.