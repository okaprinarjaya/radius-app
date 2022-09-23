-module(service_weblogin).

-export([weblogin_retrieve_device/2]).

weblogin_retrieve_device(MacAddr, AuthDeviceToken) ->
    SqlSelect_VoucherUsageDevice = "
SELECT
	vc_use_dev.voucher_usage_id,
	vc_use_dev.voucher_code,
	vc_use_dev.auth_device_token,
	vc_use_dev.mac,
	vc_use.voucher_category_id,
	vc_use.site_id,
	vc_use.start_at,
	vc_use.end_at,
	vc.max_multi_device
FROM voucher_usage_devices vc_use_dev
LEFT JOIN voucher_usages vc_use ON vc_use_dev.voucher_usage_id = vc_use.id
LEFT JOIN vouchers vc ON vc_use.voucher_code = vc.voucher_code
WHERE (vc_use_dev.mac = ? OR vc_use_dev.auth_device_token = ?)
AND vc_use_dev.deleted_at IS NULL
",
    mysql_poolboy:with(pool1, fun(Pid) ->
        {ok, _Cols0, Rows_VoucherUsageDevice} = mysql:query(Pid, SqlSelect_VoucherUsageDevice, [MacAddr, AuthDeviceToken]),

        if
            length(Rows_VoucherUsageDevice) > 0 ->
                [Row_VoucherUsageDevice|_T] = Rows_VoucherUsageDevice,
                
                VcEndAt = lists:nth(8, Row_VoucherUsageDevice),
                EndAtUnixTime = qdate:to_unixtime(VcEndAt),
                CurrentUnixTime = qdate:unixtime(),

                if 
                    EndAtUnixTime >= CurrentUnixTime ->
                        VoucherCode = lists:nth(2, Row_VoucherUsageDevice),
                        SqlSelect_VoucherReactivation = <<"SELECT * FROM voucher_reactivations WHERE voucher_code = ? AND reactivated_at IS NOT NULL AND logged_in_at IS NULL">>,
                        
                        {ok, _Cols2, Rows_VoucherReactivation} = mysql:query(Pid, SqlSelect_VoucherReactivation, [VoucherCode]),
                        {Row_VoucherUsageDevice, Rows_VoucherReactivation};

                    true -> {nok, voucher_expired}
                end;

            true -> nil
        end
    end).
