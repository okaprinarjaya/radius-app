-module(service_voucher_usages).

-export([use_voucher/1, voucher_usage/1]).

voucher_usage(VoucherCode) ->
    VoucherCode.

use_voucher(VoucherCode) ->
    if 
        VoucherCode =/= nil ->
            case retrieve_voucher(VoucherCode) of
                [VoucherCode, _DurationValue, _DurationUnit, VoucherCategoryId, SiteId] ->
                    SqlInsert_VoucherUsage = <<"INSERT INTO voucher_usages (voucher_code, voucher_category_id, site_id, start_at, end_at) VALUES (?,?,?,?,?)">>,
                    SqlInsert_VoucherUsageDevice = <<"">>,

                    mysql_poolboy:transaction(pool1, fun (Pid) ->
                        ok = mysql:query(Pid, SqlInsert_VoucherUsage , [VoucherCode, VoucherCategoryId, SiteId]),
                        ok = mysql:query(Pid, SqlInsert_VoucherUsageDevice, []),
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
    vc.site_id 
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
