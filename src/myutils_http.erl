-module(myutils_http).

-export([request_read_body/2, response_ok/3]).

response_ok(Req0, Data, Message) ->
    RespData = #{
        <<"status">> => <<"SUCCESS">>,
        <<"message">> => if Message =:= undefined -> <<"SUCCESS">>; true -> Message end,
        <<"data">> => if Data =:= undefined -> null; true -> Data end
    },
    RespJSON = jiffy:encode(RespData),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, RespJSON, Req0).

request_read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            request_read_body(Req, <<Acc/binary, Data/binary>>)
    end.
