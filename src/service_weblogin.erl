-module(service_weblogin).

-export([weblogin_retrieve_device/2, weblogin_remove_device/1]).

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
    {ok, _Cols0, Rows_VoucherUsageDevice} = mysql_poolboy:query(pool1, SqlSelect_VoucherUsageDevice, [MacAddr, AuthDeviceToken]),

    if
        length(Rows_VoucherUsageDevice) > 0 ->
            [Row_VoucherUsageDevice|_T] = Rows_VoucherUsageDevice,
            VoucherCode = lists:nth(2, Row_VoucherUsageDevice),
            
            VcEndAt = lists:nth(8, Row_VoucherUsageDevice),
            EndAtUnixTime = qdate:to_unixtime(VcEndAt),
            CurrentUnixTime = qdate:unixtime(),

            if 
                EndAtUnixTime >= CurrentUnixTime -> Row_VoucherUsageDevice;
                true -> {nok, voucher_expired, VoucherCode}
            end;

        true -> nil
    end.

weblogin_remove_device(VoucherCode) ->
    mysql_poolboy:transaction(pool1, fun (Pid) ->
        Sql = <<"UPDATE voucher_usage_devices SET deleted_at = current_timestamp() WHERE voucher_code = ?">>,
        mysql:query(Pid, Sql, [VoucherCode]),
        ok
    end).
