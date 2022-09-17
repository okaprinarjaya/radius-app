-module(service_voucher_usages).

-export([use_voucher/3, voucher_usage/3]).

voucher_usage(VoucherCode, AuthDeviceToken, MacAddr) ->
    case retrieve_voucher_usage(VoucherCode) of
        {Row_VoucherUsage, Rows_VoucherUsageDevice} ->
            [_VoucherUsageId, _VcCode, _VcCatId, _VcSiteId, _VcStartAt, _VcEndAt, MaxMultiDevice] = Row_VoucherUsage,
            if
                MaxMultiDevice =/= null andalso length(Rows_VoucherUsageDevice) > 1 ->
                    multi_device;
                true ->
                    single_device
            end;
        voucher_not_found ->
            voucher_not_found
    end.

use_voucher(VoucherCode, AuthDeviceToken, MacAddr) ->
    if 
        VoucherCode =/= nil ->
            case retrieve_voucher(VoucherCode) of
                [VoucherCode, DurationValue, DurationUnit, VoucherCategoryId, SiteId, _MaxMultiDevice] ->
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


                    mysql_poolboy:transaction(pool1, fun (Pid) ->
                        ok = mysql:query(Pid, SqlInsert_VoucherUsage , [VoucherCode, VoucherCategoryId, SiteId, StartAt, EndAt]),
                        LastInsertId = mysql:insert_id(Pid),
                        ok = mysql:query(Pid, SqlInsert_VoucherUsageDevice, [LastInsertId, VoucherCode, AuthDeviceToken, MacAddr]),
                        ok
                    end);
                voucher_not_found ->
                    voucher_not_found
            end;
        true ->
            voucher_not_found
    end.

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
            Row;
        true ->
            voucher_not_found
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

                SqlSelect_VoucherUsageDevice = <<"SELECT * FROM voucher_usage_devices WHERE voucher_usage_id = ? AND voucher_code = ?">>,
                {ok, _Cols1, Rows_VoucherUsageDevice} = mysql:query(Pid, SqlSelect_VoucherUsageDevice, [VoucherUsageId, VoucherCode]),
                {Row_VoucherUsage, Rows_VoucherUsageDevice};
            true ->
                voucher_not_found
        end
    end).
