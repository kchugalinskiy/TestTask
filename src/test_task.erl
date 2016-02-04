-module(test_task).
-export([out/1, start/0]).

-include_lib("eunit/include/eunit.hrl").
-include("yaws_api.hrl").
-include("state.hrl").

start() ->
	resource_server:start_link().

out(A) ->
	Request = A#arg.req,
	RequestMethod = Request#http_request.method,
	{_, RelPath} = Request#http_request.path,
	RequestPath = string:tokens(RelPath, "/"),
	QueryData = A#arg.querydata,
	dispatcher:dispatch_path(RequestMethod, RequestPath, QueryData).