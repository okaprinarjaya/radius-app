-module(handler_voucher_reactivations).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    method_handler(cowboy_req:method(Req0), Req0, State).

terminate(_Reason, _Req0, _State) ->
    ok.

method_handler(<<"POST">>, Req0, State) ->
    ReqData = myutils_http:request_read_body_json(Req0, <<"">>),

    VoucherCode = maps:get(<<"voucherCode">>, ReqData),

    case check_voucher_exists(VoucherCode) of
        {exists, active} ->
            case check_double_reactivation(VoucherCode) of
                nil ->
                    quickrand:seed(),

                    ReactivationToken = uuid:uuid_to_string(uuid:get_v4_urandom()),
                    NewAuthDeviceToken = uuid:uuid_to_string(uuid:get_v4_urandom()),
                    InsertValuesParam = [VoucherCode, ReactivationToken, NewAuthDeviceToken],

                    SqlInsertStr = <<"INSERT INTO voucher_reactivations (voucher_code, reactivation_token, new_auth_device_token) VALUES (?, ?, ?)">>,
                    ok = mysql_poolboy:query(pool1, SqlInsertStr , InsertValuesParam),

                    RespData = #{<<"url">> => iolist_to_binary([<<"http://localhost:8080/web/voucher-reactivation?id=">>, ReactivationToken])},
                    {ok, myutils_http:response_created(Req0, RespData, undefined), State};
                exists ->
                    {ok, myutils_http:response_badrequest(Req0, undefined, <<"Voucher reactivation already requested">>), State}
            end;
        {exists, expired} ->
            {ok, myutils_http:response_badrequest(Req0, undefined, <<"Voucher expired">>), State};
        nil ->
            {ok, myutils_http:response_notfound(Req0, undefined, <<"Voucher not found">>), State}
    end;

method_handler(_, Req0, State) ->
    {ok, myutils_http:response_notfound(Req0, undefined, undefined), State}.

check_voucher_exists(VoucherCode) ->
    SqlSelect = <<"SELECT * FROM voucher_usages WHERE voucher_code = ?">>,
    {ok, _Cols, Rows} = mysql_poolboy:query(pool1, SqlSelect, [VoucherCode]),

    if
        length(Rows) > 0 ->
            [Row|_Tail] = Rows,
            EndAtUnixTime = qdate:to_unixtime(lists:nth(6, Row)),
            CurrentUnixTime = qdate:unixtime(),

            if
                EndAtUnixTime >= CurrentUnixTime -> {exists, active};
                true -> {exists, expired}
            end;
        true -> nil
    end.

check_double_reactivation(VoucherCode) ->
    SqlSelect = <<"SELECT * FROM voucher_reactivations WHERE voucher_code = ? AND reactivated_at IS NULL AND logged_in_at IS NULL">>,
    {ok, _Cols, Rows} = mysql_poolboy:query(pool1, SqlSelect, [VoucherCode]),
    if
        length(Rows) > 0 -> exists;
        true -> nil
    end.
