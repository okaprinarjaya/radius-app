-module(handler_web_login).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    MacAddr = myutils_http:request_read_querystring(Req0, mac),
    AuthDeviceToken = get_auth_device_token(Req0),

    case service_voucher_usages:retrieve_device(MacAddr, AuthDeviceToken) of
        {Rows_VoucherUsageDevice, _Rows_VoucherReactivation} ->
            VoucherCode = lists:nth(2, Rows_VoucherUsageDevice),
            Params = #{
                "mac" => myutils_http:request_read_querystring(Req0, mac),
                "ip" => myutils_http:request_read_querystring(Req0, ip),
                "username" => VoucherCode,
                "password" => AuthDeviceToken,
                "link_login" => myutils_http:request_read_querystring(Req0, 'link-login'),
                "link_orig" => myutils_http:request_read_querystring(Req0, 'link-orig'),
                "error" => myutils_http:request_read_querystring(Req0, error),
                "trial" => myutils_http:request_read_querystring(Req0, trial),
                "chap_id" => myutils_http:request_read_querystring(Req0, 'chap-id'),
                "chap_challenge" => myutils_http:request_read_querystring(Req0, 'chap-challenge'),
                "link_login_only" => myutils_http:request_read_querystring(Req0, 'link-login-only'),
                "link_orig_esc" => myutils_http:request_read_querystring(Req0, 'link-orig-esc'),
                "mac_esc" => myutils_http:request_read_querystring(Req0, 'mac-esc'),
                "identity" => myutils_http:request_read_querystring(Req0, identity),
                "bytes_in_nice" => myutils_http:request_read_querystring(Req0, 'bytes-in-nice'),
                "bytes_out_nice" => myutils_http:request_read_querystring(Req0, 'bytes-out-nice'),
                "session_time_left" => myutils_http:request_read_querystring(Req0, 'session-time-left'),
                "uptime" => myutils_http:request_read_querystring(Req0, uptime),
                "refresh_timeout" => myutils_http:request_read_querystring(Req0, 'refresh-timeout'),
                "link_status" => myutils_http:request_read_querystring(Req0, 'link-status')
            },

            Tpl0 = bbmustache:parse_file(code:priv_dir(erl_app_oprex1) ++ "/webpage_templates/main.html"),
            TplCompile0 = bbmustache:compile(Tpl0, Params),

            nil;
        nil ->
            Params = #{
                "mac" => myutils_http:request_read_querystring(Req0, mac),
                "ip" => myutils_http:request_read_querystring(Req0, ip),
                "username" => myutils_http:request_read_querystring(Req0, username),
                "link_login" => myutils_http:request_read_querystring(Req0, 'link-login'),
                "link_orig" => myutils_http:request_read_querystring(Req0, 'link-orig'),
                "error" => myutils_http:request_read_querystring(Req0, error),
                "trial" => myutils_http:request_read_querystring(Req0, trial),
                "chap_id" => myutils_http:request_read_querystring(Req0, 'chap-id'),
                "chap_challenge" => myutils_http:request_read_querystring(Req0, 'chap-challenge'),
                "link_login_only" => myutils_http:request_read_querystring(Req0, 'link-login-only'),
                "link_orig_esc" => myutils_http:request_read_querystring(Req0, 'link-orig-esc'),
                "mac_esc" => myutils_http:request_read_querystring(Req0, 'mac-esc'),
                "identity" => myutils_http:request_read_querystring(Req0, identity),
                "bytes_in_nice" => myutils_http:request_read_querystring(Req0, 'bytes-in-nice'),
                "bytes_out_nice" => myutils_http:request_read_querystring(Req0, 'bytes-out-nice'),
                "session_time_left" => myutils_http:request_read_querystring(Req0, 'session-time-left'),
                "uptime" => myutils_http:request_read_querystring(Req0, uptime),
                "refresh_timeout" => myutils_http:request_read_querystring(Req0, 'refresh-timeout'),
                "link_status" => myutils_http:request_read_querystring(Req0, 'link-status')
            },

            Tpl0 = bbmustache:parse_file(code:priv_dir(erl_app_oprex1) ++ "/webpage_templates/main.html"),
            TplCompile0 = bbmustache:compile(Tpl0, Params),

            nil
    end,

    Tpl = bbmustache:parse_file(code:priv_dir(erl_app_oprex1) ++ "/webpage_templates/main.html"),
    TplCompile = bbmustache:compile(Tpl, #{"title" => "Wiqu Login Voucher"}),

    Cookies = cowboy_req:parse_cookies(Req0),
    case lists:keyfind(<<"wiqu_auth_device_token">>, 1, Cookies) of
        {_, AuthDeviceToken} ->
            io:format("Cookie: ~p~n", [AuthDeviceToken]),

            Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, TplCompile, Req0),
            {ok, Req1, State};
        false ->
            io:format("Cookie expired maybe. Then set new cookie~n"),

            Req1 = cowboy_req:set_resp_cookie(<<"wiqu_auth_device_token">>, <<"123321">>, Req0,  #{max_age => 60 * 3}),
            Req2 = cowboy_req:set_resp_cookie(<<"wiqu_vc_code">>, <<"_A_B_C_">>, Req1,  #{max_age => 60 * 3}),
            Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, TplCompile, Req2),
            {ok, Req3, State}
    end.

terminate(_A, _B, _C) -> ok.

get_auth_device_token(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(<<"wiqu_auth_device_token">>, 1, Cookies) of
        {_, AuthDeviceToken} -> AuthDeviceToken;
        false -> nil
    end.
