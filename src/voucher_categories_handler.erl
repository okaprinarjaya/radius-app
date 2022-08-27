-module(voucher_categories_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    method_handler(Method, Req0, State).

terminate(_A, _B, _C) -> ok.

method_handler(<<"GET">>, Req0, State) ->
    {ok, myutils_http:response_ok(Req0, undefined, <<"Hello world! From GET">>), State};
method_handler(<<"POST">>, Req0, State) ->
    ReqData = myutils_http:request_read_body_json(Req0, <<"">>),
    InsertValuesParam = [
        maps:get(<<"categoryName">>, ReqData),
        maps:get(<<"priceBasic">>, ReqData),
        maps:get(<<"durationValue">>, ReqData),
        maps:get(<<"durationUnit">>, ReqData),
        maps:get(<<"createdBy">>, ReqData)
    ],
    SqlInsertStr = <<"INSERT INTO voucher_categories (category_name, price_basic, duration_value, duration_unit, created_by) VALUES (?, ?, ?, ?, ?)">>,
    mysql_poolboy:query(pool1, SqlInsertStr , InsertValuesParam),

    RespData = #{<<"hello">> => <<"world!">>, <<"attrs">> => #{<<"foo">> => <<"bar">>}},
    {ok, myutils_http:response_created(Req0, RespData, undefined), State};
method_handler(_, Req0, State) ->
    {ok, myutils_http:response_ok(Req0, undefined, <<"Hello world! From ANY other than GET POST">>), State}.
