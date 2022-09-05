-module(handler_hello).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, <<"Hello World!">>, Req0),
    {ok, Req, State}.

terminate(_A, _B, _C) -> ok.
