-module(json_handler).

-export([init/2, terminate/3, allowed_methods/2, content_types_provided/2, content_types_accepted/2, myhandler/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    io:format("Masuk content_types_provided()~n"),
    {
        [
            {{<<"application">>, <<"json">>, []}, myhandler}
        ],
        Req,
        State
    }.

content_types_accepted(Req, State) ->
    io:format("Masuk content_types_accepted()~n"),
    {
        [
            {{<<"application">>, <<"json">>, []}, myhandler}
        ],
        Req,
        State
    }.

terminate(_A, _B, _C) -> ok.

myhandler(Req, Something) ->
    io:format("Masuk (Req, Something)~n"),
    case cowboy_req:method(Req) of
        <<"POST">> ->
            true = cowboy_req:has_body(Req),
            {ok, ReqBody, _Req} = read_body(Req, <<"">>),
            ReqData = jiffy:decode(ReqBody, [return_maps]),

            io:format("~p~n", [ReqBody]),
            io:format("~p~n", [ReqData]),

            maps:foreach(fun (K,V) -> io:format("~s => ~s~n", [K, V]) end, ReqData),

            {{created, "/a/b/123"}, Req, Something};
        _ ->
            io:format("Masuk ANYTHING~n"),
            JSON = jiffy:encode({[{foo, [<<"bing">>, 2.3, true]}]}),
            {JSON, Req, Something}
    end.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.
