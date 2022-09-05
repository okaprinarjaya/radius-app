-module(handler_voucher_categories).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    method_handler(cowboy_req:method(Req0), Req0, State).

terminate(_Reason, _Req0, _State) ->
    ok.

method_handler(<<"GET">>, Req0, State) ->
    UserAuthInfo = maps:get(user_auth_info, Req0),
    RespMsg = [
        <<"Hello!">>,
        <<" ">>,
        maps:get(<<"userName">>, UserAuthInfo), 
        <<", with ID: ">>, 
        maps:get(<<"userId">>, UserAuthInfo), 
        <<", Role: ">>,
        maps:get(<<"role">>, UserAuthInfo)
    ],

    {ok, myutils_http:response_ok(Req0, undefined, iolist_to_binary(RespMsg)), State};

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
    ok = mysql_poolboy:query(pool1, SqlInsertStr , InsertValuesParam),

    RespData = #{<<"hello">> => <<"world!">>, <<"attrs">> => #{<<"foo">> => <<"bar">>}},
    {ok, myutils_http:response_created(Req0, RespData, undefined), State};

method_handler(_, Req0, State) ->
    {ok, myutils_http:response_notfound(Req0, undefined, undefined), State}.
