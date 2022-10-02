-module(handler_api_voucher_usages).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    method_handler(cowboy_req:method(Req0), Req0, State).

terminate(_Reason, _Req0, _State) ->
    ok.

method_handler(<<"POST">>, Req0, State) ->
    case cowboy_req:path(Req0) of
        <<"/api/voucher-usages/_home-customer-register">> ->
            handle_voucher_usage_home_customer_register(Req0, State);
        <<"/api/voucher-usages">> ->
            handle_voucher_usage(Req0, State)
    end;

method_handler(<<"DELETE">>, Req0, State) ->
    ReqData = myutils_http:request_read_body_json(Req0, <<"">>),
    VoucherCode = maps:get(<<"voucherCode">>, ReqData),

    case service_voucher:retrieve_voucher(VoucherCode) of
        {voucher_found, _RowVoucher} ->
            case service_voucher:retrieve_voucher_usage(VoucherCode) of
                {Row_VoucherUsage, _Rows_VoucherUsageDevice, _Rows_VoucherReactivation} ->
                    {atomic, ok} = mysql_poolboy:transaction(pool1, fun (Pid) ->
                        VcUsageId = lists:nth(1, Row_VoucherUsage),

                        Sql1 = <<"UPDATE voucher_usages SET deleted_at = current_timestamp() WHERE id = ? AND deleted_at IS NULL">>,
                        Sql2 = <<"UPDATE voucher_usage_devices SET deleted_at = current_timestamp() WHERE voucher_usage_id = ? AND deleted_at IS NULL">>,
                        ok = mysql:query(Pid, Sql1 , [VcUsageId]),
                        ok = mysql:query(Pid, Sql2, [VcUsageId]),
                        ok
                    end),
                    {ok, myutils_http:response_ok(Req0, undefined, undefined), State};
                voucher_expired ->
                    {ok, myutils_http:response_badrequest(Req0, undefined, <<"Voucher expired">>), State};
                voucher_unused ->
                    {ok, myutils_http:response_badrequest(Req0, undefined, <<"Voucher unused">>), State}
            end;

        voucher_not_found ->
            {ok, myutils_http:response_notfound(Req0, undefined, <<"Voucher not found">>), State}
    end;

method_handler(_, Req0, State) ->
    {ok, myutils_http:response_notfound(Req0, undefined, undefined), State}.

handle_voucher_usage(Req0, State) ->
    {ok, myutils_http:response_ok(Req0, undefined, undefined), State}.

handle_voucher_usage_home_customer_register(Req0, State) ->
    quickrand:seed(),

    ReqData = myutils_http:request_read_body_json(Req0, <<"">>),
    VoucherCode = maps:get(<<"voucherCode">>, ReqData),
    MacAddr = maps:get(<<"macAddr">>, ReqData),
    AuthDeviceToken = uuid:uuid_to_string(uuid:get_v4_urandom()),

    case service_voucher:use_voucher(VoucherCode, MacAddr, AuthDeviceToken) of
        {ok, new_registered} ->
            {ok, myutils_http:response_created(Req0, undefined, undefined), State};
        {nok, voucher_not_found} ->
            {ok, myutils_http:response_notfound(Req0, undefined, <<"Voucher not found">>), State}
    end.
