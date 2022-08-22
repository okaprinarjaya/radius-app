-module(hello_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    {ok, IOList} = template_compiler:render("main.html", #{<<"a">> => 1}, [], undefined),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html; charset=utf-8">>},
                           IOList,
                           Req0),
    {ok, Req, State}.

terminate(_A, _B, _C) -> ok.
