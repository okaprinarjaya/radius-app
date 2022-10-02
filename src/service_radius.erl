-module(service_radius).

-export([radius_auth/3]).

radius_auth(VoucherCode, MacAddr, AuthDeviceToken) ->
    case radius_voucher_usage(VoucherCode) of
        {ok, voucher_used, single_device, {VcUsageId, Rows_VoucherUsageDevice, Rows_VoucherReactivation}} ->
            DeviceRegistered = lists:search(
                fun (T) -> lists:nth(5, T) =:= MacAddr orelse lists:nth(4, T) =:= AuthDeviceToken end,
                Rows_VoucherUsageDevice
            ),
            case DeviceRegistered of
                {value, _Device} -> {ok, registered};
                false ->
                    if
                        length(Rows_VoucherReactivation) > 0 ->
                            Params = [
                                {vc_code, VoucherCode},
                                {vc_usage_id, VcUsageId},
                                {mac_addr, MacAddr},
                                {auth_device_token, AuthDeviceToken},
                                {vc_reactivation, lists:nth(1, Rows_VoucherReactivation)}
                            ],
                            radius_voucher_reactivation(Params);
                        true -> {nok, unknown_device}
                    end
            end;
        {ok, voucher_used, multi_device, {MaxMultiDevice, VcUsageId, Rows_VoucherUsageDevice, _Rows_VoucherReactivation}} ->
            DeviceRegistered = lists:search(
                fun (T) -> lists:nth(5, T) =:= MacAddr orelse lists:nth(4, T) =:= AuthDeviceToken end,
                Rows_VoucherUsageDevice
            ),
            case DeviceRegistered of
                {value, _Device} -> {ok, registered};
                false ->
                    Params = [
                        {vc_usage_id, VcUsageId},
                        {mac_addr, MacAddr},
                        {auth_device_token, AuthDeviceToken},
                        {vc_code, VoucherCode},
                        {max_multi_device, MaxMultiDevice},
                        {total_devices, length(Rows_VoucherUsageDevice)}
                    ],
                    radius_use_same_voucher_for_new_device(Params)
            end;
        {ok, voucher_unused} -> service_voucher:use_voucher(VoucherCode, MacAddr, AuthDeviceToken);
        {nok, voucher_expired} -> {nok, voucher_expired}
    end.

radius_voucher_usage(VoucherCode) ->
    case service_voucher:retrieve_voucher_usage(VoucherCode) of
        {Row_VoucherUsage, Rows_VoucherUsageDevice, Rows_VoucherReactivation} ->
            VcUsageId = lists:nth(1, Row_VoucherUsage),
            MaxMultiDevice = lists:nth(7, Row_VoucherUsage),

            if
                MaxMultiDevice =/= null ->
                    {ok, voucher_used, multi_device, {MaxMultiDevice, VcUsageId, Rows_VoucherUsageDevice, Rows_VoucherReactivation}};
                true ->
                    {ok, voucher_used, single_device, {VcUsageId, Rows_VoucherUsageDevice, Rows_VoucherReactivation}}
            end;
        voucher_expired -> {nok, voucher_expired};
        voucher_unused -> {ok, voucher_unused}
    end.

radius_use_same_voucher_for_new_device(Params) ->
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

            Sql = <<"INSERT INTO voucher_usage_devices (voucher_usage_id, voucher_code, auth_device_token, mac) VALUES (?,?,?,?)">>,
            ok = mysql_poolboy:query(pool1, Sql , [VoucherUsageId, VoucherCode, AuthDeviceToken, MacAddr]),
            {ok, new_registered}
    end.

radius_voucher_reactivation(Params) ->
    {vc_reactivation, VoucherReactivation} = lists:keyfind(vc_reactivation, 1, Params),
    {vc_usage_id, VcUsageId} = lists:keyfind(vc_usage_id, 1, Params),
    {auth_device_token, AuthDeviceToken} = lists:keyfind(auth_device_token, 1, Params),
    {vc_code, VoucherCode} = lists:keyfind(vc_code, 1, Params),
    {mac_addr, MacAddr} = lists:keyfind(mac_addr, 1, Params),

    NewAuthDeviceToken = lists:nth(4, VoucherReactivation),
    if
        AuthDeviceToken =:= NewAuthDeviceToken ->
            {atomic, ok} = mysql_poolboy:transaction(pool1, fun (Pid) ->
                Sql1 = <<"UPDATE voucher_usage_devices SET deleted_at = current_timestamp() WHERE voucher_usage_id = ? AND voucher_code = ?">>,
                Sql2 = <<"INSERT INTO voucher_usage_devices (voucher_usage_id, voucher_code, auth_device_token, mac) VALUES (?,?,?,?)">>,
                Sql3 = <<"UPDATE voucher_reactivations SET logged_in_at = current_timestamp() WHERE voucher_code = ? AND new_auth_device_token = ?">>,

                mysql:query(Pid, Sql1, [VcUsageId, VoucherCode]),
                mysql:query(Pid, Sql2, [VcUsageId, VoucherCode, AuthDeviceToken, MacAddr]),
                mysql:query(Pid, Sql3, [VoucherCode, NewAuthDeviceToken]),
                ok
            end),

            {ok, re_registered};
        true -> {nok, unknown_device}
    end.
