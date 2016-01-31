-module(test_task).
-export([out/1]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/yaws_api.hrl").

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

out(A) ->
    Request = A#arg.req,
    RequestMethod = Request#http_request.method,
    {ehtml,
     [{p,[],
       box(io_lib:format("A#arg.pathinfo = ~p~n"
                         "RequestMethod = ~p~n"
                         "Request = ~p~n"
                         "A = ~p~n",
                         [A#arg.pathinfo,
                          RequestMethod,
                          Request,
                          A]))}]}.