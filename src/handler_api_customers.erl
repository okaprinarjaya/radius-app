-module(handler_api_customers).

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
    UserAuthInfo = maps:get(user_auth_info, Req0),
    UserId = maps:get(<<"userId">>, UserAuthInfo),

    ReqData = myutils_http:request_read_body_json(Req0, <<"">>),
    InsertValuesParam = [
        maps:get(<<"voucherCode">>, ReqData),
        maps:get(<<"macAddr">>, ReqData),
        maps:get(<<"siteId">>, ReqData),
        maps:get(<<"customerName">>, ReqData),
        UserId
    ],

    Sql = <<"INSERT INTO customers (voucher_code, mac, site_id, customer_name, created_by) VALUES (?, ?, ?, ?, ?)">>,
    ExecSql = mysql_poolboy:query(pool1, Sql , InsertValuesParam),

    case ExecSql of
        ok ->
            {ok, myutils_http:response_created(Req0, undefined, undefined), State};
        {error,{_ErrCode1,_ErrCode2,ErrMessage}} ->
            logger:error("Error occured because: ~p", [ErrMessage]),
            {ok, myutils_http:response_conflict(Req0, undefined, <<"Data conflict">>), State}
    end;

method_handler(_, Req0, State) ->
    {ok, myutils_http:response_notfound(Req0, undefined, undefined), State}.
