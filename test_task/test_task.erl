-module(test_task).
-export([out/1, start/0]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/yaws_api.hrl").
	
start() ->
	resource_server:start_link().

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

out(A) ->
    Reply = resource_server:allocate("Alice"),
    Request = A#arg.req,
    OldState = A#arg.state,
    NewArg = A#arg{ state = 1 },
    RequestMethod = Request#http_request.method,
    %http://stackoverflow.com/questions/7864784/how-to-maintain-stateful-in-yaws
    {ehtml,
     [{p,[],
       box(io_lib:format("Reply = ~p~n"
                         "A#arg.pathinfo = ~p~n"
                         "RequestMethod = ~p~n"
                         "Request = ~p~n"
                         "State = ~p~n"
                         "OldState = ~p~n"
                         "A = ~p~n",
                         [Reply,
                          A#arg.pathinfo,
                          RequestMethod,
                          Request,
                          A#arg.state,
                          OldState,
                          A]))}]}.