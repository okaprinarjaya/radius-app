-module(handler_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req0, Env) ->
    io:format("Method: ~p~n", [cowboy_req:method(Req0)]),
    {ok, Req0, Env}.
