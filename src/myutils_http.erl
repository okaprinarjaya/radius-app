-module(myutils_http).

-export([request_read_body_json/2, response_ok/3, response_created/3]).

response_ok(Req0, Data, Message) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, resp_data_json(Data, Message), Req0).

response_created(Req0, Data, Message) ->
    cowboy_req:reply(201, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, resp_data_json(Data, Message), Req0).

resp_data_json(Data, Message) ->
    RespData = #{
        <<"status">> => <<"SUCCESS">>,
        <<"message">> => if Message =:= undefined -> <<"SUCCESS">>; true -> Message end,
        <<"data">> => if Data =:= undefined -> null; true -> Data end
    },
    jiffy:encode(RespData).

request_read_body_json(Req0, Acc) ->
    true = cowboy_req:has_body(Req0),
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} ->
            jiffy:decode(Data, [return_maps]);
        {more, Data, Req} ->
            request_read_body_json(Req, <<Acc/binary, Data/binary>>)
    end.
