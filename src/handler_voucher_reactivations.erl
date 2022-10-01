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

    case service_voucher:retrieve_voucher_usage(VoucherCode) of
        {_Row_VoucherUsage, _Rows_VoucherUsageDevice, _Rows_VoucherReactivation} ->
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
        voucher_expired ->
            {ok, myutils_http:response_badrequest(Req0, undefined, <<"Voucher expired">>), State};
        voucher_unused ->
            {ok, myutils_http:response_notfound(Req0, undefined, <<"Voucher not found / unused">>), State}
    end;

method_handler(_, Req0, State) ->
    {ok, myutils_http:response_notfound(Req0, undefined, undefined), State}.

check_double_reactivation(VoucherCode) ->
    SqlSelect = <<"SELECT * FROM voucher_reactivations WHERE voucher_code = ? AND reactivated_at IS NULL AND logged_in_at IS NULL">>,
    {ok, _Cols, Rows} = mysql_poolboy:query(pool1, SqlSelect, [VoucherCode]),
    if
        length(Rows) > 0 -> exists;
        true -> nil
    end.
