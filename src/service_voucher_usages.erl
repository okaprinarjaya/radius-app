-module(service_voucher_usages).

-export([use_voucher/3, voucher_usage/1, login/3]).

login(VoucherCode, AuthDeviceToken, MacAddr) ->
    case voucher_usage(VoucherCode) of
        {ok, voucher_used, single_device, {Rows_VoucherUsageDevice}} ->
            DeviceRegistered = lists:search(
                fun (T) ->  lists:nth(5, T) =:= MacAddr orelse lists:nth(4, T) =:= AuthDeviceToken end,
                Rows_VoucherUsageDevice
            ),
            case DeviceRegistered of
                {value, _Device} -> {ok, registered};
                false ->
                    % {nok, unknown_device}
                    maybe_voucher_reactivation()
            end;
        {ok, voucher_used, multi_device, {MaxMultiDevice, VcUsageId, Rows_VoucherUsageDevice}} ->
            DeviceRegistered = lists:search(
                fun (T) ->  lists:nth(5, T) =:= MacAddr orelse lists:nth(4, T) =:= AuthDeviceToken end,
                Rows_VoucherUsageDevice
            ),
            case DeviceRegistered of
                {value, _Device} -> {ok, registered};
                false ->
                    Params = [
                        {vc_usage_id, VcUsageId},
                        {auth_device_token, AuthDeviceToken},
                        {vc_code, VoucherCode},
                        {mac_addr, MacAddr},
                        {max_multi_device, MaxMultiDevice},
                        {total_devices, length(Rows_VoucherUsageDevice)}
                    ],
                    use_same_voucher_for_new_device(Params)
            end;
        {ok, voucher_unused} -> use_voucher(VoucherCode, AuthDeviceToken, MacAddr);
        {nok, voucher_expired} -> {nok, voucher_expired}
    end.

voucher_usage(VoucherCode) ->
    case retrieve_voucher_usage(VoucherCode) of
        {[VcUsageId, _VcCode, _VcCatId, _VcSiteId, _VcStartAt, VcEndAt, MaxMultiDevice], Rows_VoucherUsageDevice} ->
            EndAtUnixTime = qdate:to_unixtime(VcEndAt),
            CurrentUnixTime = qdate:unixtime(),
            if 
                EndAtUnixTime >= CurrentUnixTime ->
                    if
                        MaxMultiDevice =/= null ->
                            {ok, voucher_used, multi_device, {MaxMultiDevice, VcUsageId, Rows_VoucherUsageDevice}};
                        true ->
                            {ok, voucher_used, single_device, {Rows_VoucherUsageDevice}}
                    end;
                true -> {nok, voucher_expired}
            end;
        voucher_unused -> {ok, voucher_unused}
    end.

use_voucher(VoucherCode, AuthDeviceToken, MacAddr) ->
    if 
        VoucherCode =/= nil ->
            case retrieve_voucher(VoucherCode) of
                {voucher_found, [VoucherCode, DurationValue, DurationUnit, VoucherCategoryId, SiteId, _MaxMultiDevice]} ->
                    SqlInsert_VoucherUsage = <<"INSERT INTO voucher_usages (voucher_code, voucher_category_id, site_id, start_at, end_at) VALUES (?,?,?,?,?)">>,
                    SqlInsert_VoucherUsageDevice = <<"INSERT INTO voucher_usage_devices (voucher_usage_id, voucher_code, auth_device_token, mac) VALUES (?,?,?,?)">>,

                    StartAt = qdate:to_date(qdate:unixtime()),
                    EndAt = case DurationUnit of
                        <<"HOUR">> ->
                            qdate:to_date(qdate:add_hours(DurationValue, StartAt));
                        <<"DAY">> ->
                            qdate:to_date(qdate:add_days(DurationValue, StartAt));
                        <<"WEEK">> ->
                            qdate:to_date(qdate:add_weeks(DurationValue, StartAt));
                        <<"MONTH">> ->
                            qdate:to_date(qdate:add_months(DurationValue, StartAt))
                    end,


                    {atomic, ok} = mysql_poolboy:transaction(pool1, fun (Pid) ->
                        ok = mysql:query(Pid, SqlInsert_VoucherUsage , [VoucherCode, VoucherCategoryId, SiteId, StartAt, EndAt]),
                        LastInsertId = mysql:insert_id(Pid),
                        ok = mysql:query(Pid, SqlInsert_VoucherUsageDevice, [LastInsertId, VoucherCode, AuthDeviceToken, MacAddr]),
                        ok
                    end),
                    {ok, new_registered};

                voucher_not_found -> {nok, voucher_not_found}
            end;
        true -> {nok, voucher_not_found}
    end.

use_same_voucher_for_new_device(Params) ->
    {max_multi_device, MaxMultiDevice} = lists:keyfind(max_multi_device, 1, Params),
    {total_devices, TotalDevices} = lists:keyfind(total_devices, 1, Params),

    if
        TotalDevices =:= MaxMultiDevice ->
            {nok, max_multi_device_exceeded};
        true ->
            {vc_usage_id, VoucherUsageId} = lists:keyfind(vc_usage_id, 1, Params),
            {vc_code, VoucherCode} = lists:keyfind(vc_code, 1, Params),
            {auth_device_token, AuthDeviceToken} = lists:keyfind(auth_device_token, 1, Params),
            {mac_addr, MacAddr} = lists:keyfind(mac_addr, 1, Params),
            
            SqlInsert_VoucherUsageDevice = <<"INSERT INTO voucher_usage_devices (voucher_usage_id, voucher_code, auth_device_token, mac) VALUES (?,?,?,?)">>,
            ok = mysql_poolboy:query(pool1, SqlInsert_VoucherUsageDevice , [VoucherUsageId, VoucherCode, AuthDeviceToken, MacAddr]),
            {ok, new_registered}
    end.

maybe_voucher_reactivation() ->
    nil.

retrieve_voucher(VoucherCode) ->
    SqlSelect_Voucher = "
SELECT
    vc.voucher_code,
    vc_cat.duration_value,
    vc_cat.duration_unit,
    vc.voucher_category_id,
    vc.site_id,
    vc.max_multi_device 
FROM
    vouchers vc
    LEFT JOIN voucher_categories vc_cat ON vc.voucher_category_id = vc_cat.id 
WHERE
    vc.voucher_code = ?
",
    {ok, _Cols, Rows} = mysql_poolboy:query(pool1, SqlSelect_Voucher, [VoucherCode]),
    if 
        length(Rows) > 0 ->
            [Row|_T] = Rows,
            {voucher_found, Row};
        true -> voucher_not_found
    end.

retrieve_voucher_usage(VoucherCode) ->
    SqlSelect_VoucherUsage = "
SELECT
	vc_use.id,
	vc_use.voucher_code,
	vc_use.voucher_category_id,
	vc_use.site_id,
	vc_use.start_at,
	vc_use.end_at,
	vc.max_multi_device 
FROM
	voucher_usages vc_use
	LEFT JOIN vouchers vc ON vc_use.voucher_code = vc.voucher_code 
WHERE
	vc_use.voucher_code = ?
",
    mysql_poolboy:with(pool1, fun(Pid) ->
        {ok, _Cols0, Rows_VoucherUsage} = mysql:query(Pid, SqlSelect_VoucherUsage, [VoucherCode]),

        if
            length(Rows_VoucherUsage) > 0 ->
                [Row_VoucherUsage|_T] = Rows_VoucherUsage,
                [VoucherUsageId, _, _, _, _, _, _] = Row_VoucherUsage,

                SqlSelect_VoucherUsageDevice = <<"SELECT * FROM voucher_usage_devices WHERE voucher_usage_id = ? AND voucher_code = ? AND deleted_at IS NULL">>,
                {ok, _Cols1, Rows_VoucherUsageDevice} = mysql:query(Pid, SqlSelect_VoucherUsageDevice, [VoucherUsageId, VoucherCode]),
                {Row_VoucherUsage, Rows_VoucherUsageDevice};
            true -> voucher_unused
        end
    end).
