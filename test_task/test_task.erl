-module(test_task).
-export([out/1]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/yaws_api.hrl").

-record(task_state, {r1, r2, r3}).

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

out(A) ->
    Request = A#arg.req,
    OldState = A#arg.state,
    NewArg = A#arg{ state = 1 },
    RequestMethod = Request#http_request.method,
    %http://stackoverflow.com/questions/7864784/how-to-maintain-stateful-in-yaws
    {ehtml,
     [{p,[],
       box(io_lib:format("A#arg.pathinfo = ~p~n"
                         "RequestMethod = ~p~n"
                         "Request = ~p~n"
                         "State = ~p~n"
                         "OldState = ~p~n"
                         "A = ~p~n",
                         [A#arg.pathinfo,
                          RequestMethod,
                          Request,
                          A#arg.state,
                          OldState,
                          A]))}]}.