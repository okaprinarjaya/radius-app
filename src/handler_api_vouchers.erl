-module(handler_api_vouchers).

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
    VoucherCategoryId = maps:get(<<"voucherCategoryId">>, ReqData),
    SiteId = maps:get(<<"siteId">>, ReqData),
    MaxMultiDevice = maps:get(<<"maxMultiDevice">>, ReqData),

    Salt = myutils_identifier:generate_salt(),
    VoucherCode = myutils_identifier:generate_voucher_code(Salt, SiteId),

    InsertValuesParam = [VoucherCategoryId, VoucherCode,SiteId, MaxMultiDevice],
    SqlInsertStr = <<"INSERT INTO vouchers (voucher_category_id, voucher_code, site_id, max_multi_device, is_sold) VALUES (?, ?, ?, ?, 0)">>,
    ok = mysql_poolboy:query(pool1, SqlInsertStr , InsertValuesParam),

    {ok, myutils_http:response_created(Req0, undefined, undefined), State};

method_handler(_, Req0, State) ->
    {ok, myutils_http:response_notfound(Req0, undefined, undefined), State}.
