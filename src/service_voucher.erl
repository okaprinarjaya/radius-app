-module(service_voucher).

-export([retrieve_voucher/1, retrieve_voucher_usage/1]).

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
    Sql = "
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
WHERE vc_use.voucher_code = ?
AND vc_use.deleted_at IS NULL
",
    mysql_poolboy:with(pool1, fun(Pid) ->
        {ok, _Cols0, Rows_VoucherUsage} = mysql:query(Pid, Sql, [VoucherCode]),

        if
            length(Rows_VoucherUsage) > 0 ->
                [Row_VoucherUsage|_T] = Rows_VoucherUsage,

                VcEndAt = lists:nth(6, Row_VoucherUsage),
                EndAtUnixTime = qdate:to_unixtime(VcEndAt),
                CurrentUnixTime = qdate:unixtime(),

                if
                    EndAtUnixTime >= CurrentUnixTime ->
                        VoucherUsageId = lists:nth(1, Row_VoucherUsage),

                        Sql1 = <<"SELECT * FROM voucher_usage_devices WHERE voucher_usage_id = ? AND voucher_code = ? AND deleted_at IS NULL">>,
                        Sql2 = <<"SELECT * FROM voucher_reactivations WHERE voucher_code = ? AND reactivated_at IS NOT NULL AND logged_in_at IS NULL">>,

                        {ok, _Cols1, Rows_VoucherUsageDevice} = mysql:query(Pid, Sql1, [VoucherUsageId, VoucherCode]),
                        {ok, _Cols2, Rows_VoucherReactivation} = mysql:query(Pid, Sql2, [VoucherCode]),

                        {Row_VoucherUsage, Rows_VoucherUsageDevice, Rows_VoucherReactivation};

                    true -> voucher_expired
                end;

            true -> voucher_unused
        end
    end).
