-module(myutils_http).

-export([request_read_body_json/2, response_ok/3, response_created/3, response_unauthorized/3, response_notfound/3]).

response_ok(Req0, Data, Message) ->
    RespData = #{
        <<"status">> => <<"SUCCESS">>,
        <<"message">> => if Message =:= undefined -> <<"Success">>; true -> Message end,
        <<"data">> => if Data =:= undefined -> null; true -> Data end
    },
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, jiffy:encode(RespData), Req0).

response_created(Req0, Data, Message) ->
    RespData = #{
        <<"status">> => <<"CREATED">>,
        <<"message">> => if Message =:= undefined -> <<"Created">>; true -> Message end,
        <<"data">> => if Data =:= undefined -> null; true -> Data end
    },
    cowboy_req:reply(201, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, jiffy:encode(RespData), Req0).

response_unauthorized(Req0, Data, Message) ->
    RespData = #{
        <<"status">> => <<"UNAUTHORIZED">>,
        <<"message">> => if Message =:= undefined -> <<"Unauthorized">>; true -> Message end,
        <<"data">> => if Data =:= undefined -> null; true -> Data end
    },
    cowboy_req:reply(401, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, jiffy:encode(RespData), Req0).

response_notfound(Req0, Data, Message) ->
    RespData = #{
        <<"status">> => <<"NOT FOUND">>,
        <<"message">> => if Message =:= undefined -> <<"Not Found">>; true -> Message end,
        <<"data">> => if Data =:= undefined -> null; true -> Data end
    },
    cowboy_req:reply(404, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, jiffy:encode(RespData), Req0).

request_read_body_json(Req0, Acc) ->
    true = cowboy_req:has_body(Req0),
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} ->
            jiffy:decode(Data, [return_maps]);
        {more, Data, Req} ->
            request_read_body_json(Req, <<Acc/binary, Data/binary>>)
    end.
