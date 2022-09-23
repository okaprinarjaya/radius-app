-module(handler_web_login).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    MacAddr = myutils_http:request_read_querystring(Req0, <<"mac">>),
    AuthDeviceTokenExisting = get_auth_device_token(Req0),
    MacAddrFixVal = if MacAddr =:= nil -> <<"">>; true -> MacAddr end,
    AuthDeviceTokenExistingFixVal = if AuthDeviceTokenExisting =:= nil -> <<"">>; true -> AuthDeviceTokenExisting end,

    case service_weblogin:weblogin_retrieve_device(MacAddrFixVal, AuthDeviceTokenExistingFixVal) of
        {nok, voucher_expired, VoucherCode} ->
            {atomic, ok} = service_weblogin:weblogin_remove_device(VoucherCode),

            Tpl0 = bbmustache:parse_file(code:priv_dir(erl_app_oprex1) ++ "/webpage_templates/login-voucher-expired.html"),
            TplCompile0 = bbmustache:compile(Tpl0, #{}),
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, TplCompile0, Req0),
            {ok, Req, State};

        nil ->
            AuthDeviceToken = get_auth_device_token(Req0),
            Params = #{
                "mac" => myutils_http:request_read_querystring(Req0, <<"mac">>),
                "ip" => myutils_http:request_read_querystring(Req0, <<"ip">>),
                "password" => AuthDeviceToken,
                "link_login" => myutils_http:request_read_querystring(Req0, <<"link-login">>),
                "link_orig" => myutils_http:request_read_querystring(Req0, <<"link-orig">>),
                "error" => myutils_http:request_read_querystring(Req0, <<"error">>),
                "trial" => myutils_http:request_read_querystring(Req0, <<"trial">>),
                "chap_id" => myutils_http:request_read_querystring(Req0, <<"chap-id">>),
                "chap_challenge" => myutils_http:request_read_querystring(Req0, <<"chap-challenge">>),
                "link_login_only" => myutils_http:request_read_querystring(Req0, <<"link-login-only">>),
                "link_orig_esc" => myutils_http:request_read_querystring(Req0, <<"link-orig-esc">>),
                "mac_esc" => myutils_http:request_read_querystring(Req0, <<"mac-esc">>),
                "identity" => myutils_http:request_read_querystring(Req0, <<"identity">>),
                "bytes_in_nice" => myutils_http:request_read_querystring(Req0, <<"bytes-in-nice">>),
                "bytes_out_nice" => myutils_http:request_read_querystring(Req0, <<"bytes-out-nice">>),
                "session_time_left" => myutils_http:request_read_querystring(Req0, <<"session-time-left">>),
                "uptime" => myutils_http:request_read_querystring(Req0, uptime),
                "refresh_timeout" => myutils_http:request_read_querystring(Req0, 'refresh-timeout'),
                "link_status" => myutils_http:request_read_querystring(Req0, 'link-status')
            },

            Tpl0 = bbmustache:parse_file(code:priv_dir(erl_app_oprex1) ++ "/webpage_templates/login.html"),

            if
                AuthDeviceToken =:= nil ->
                    quickrand:seed(),

                    ThreeMonthInSeconds = 7890000,
                    NewAuthDeviceToken = uuid:uuid_to_string(uuid:get_v4_urandom()),
                    NewParams = maps:update("password", NewAuthDeviceToken, Params),
                    TplCompile0 = bbmustache:compile(Tpl0, NewParams),

                    Req1 = cowboy_req:set_resp_cookie(<<"wiqu_auth_device_token">>, NewAuthDeviceToken, Req0,  #{max_age => ThreeMonthInSeconds}),
                    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, TplCompile0, Req1),
                    {ok, Req2, State};
                true ->
                    TplCompile0 = bbmustache:compile(Tpl0, Params),
                    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, TplCompile0, Req0),
                    {ok, Req, State}
            end;
        
        Row_VoucherUsageDevice ->
            VoucherCode = lists:nth(2, Row_VoucherUsageDevice),
            Params = #{
                "mac" => myutils_http:request_read_querystring(Req0, <<"mac">>),
                "ip" => myutils_http:request_read_querystring(Req0, <<"ip">>),
                "username" => VoucherCode,
                "password" => AuthDeviceTokenExisting,
                "link_login" => myutils_http:request_read_querystring(Req0, <<"link-login">>),
                "link_orig" => myutils_http:request_read_querystring(Req0, <<"link-orig">>),
                "error" => myutils_http:request_read_querystring(Req0, <<"error">>),
                "trial" => myutils_http:request_read_querystring(Req0, <<"trial">>),
                "chap_id" => myutils_http:request_read_querystring(Req0, <<"chap-id">>),
                "chap_challenge" => myutils_http:request_read_querystring(Req0, <<"chap-challenge">>),
                "link_login_only" => myutils_http:request_read_querystring(Req0, <<"link-login-only">>),
                "link_orig_esc" => myutils_http:request_read_querystring(Req0, <<"link-orig-esc">>),
                "mac_esc" => myutils_http:request_read_querystring(Req0, <<"mac-esc">>),
                "identity" => myutils_http:request_read_querystring(Req0, <<"identity">>),
                "bytes_in_nice" => myutils_http:request_read_querystring(Req0, <<"bytes-in-nice">>),
                "bytes_out_nice" => myutils_http:request_read_querystring(Req0, <<"bytes-out-nice">>),
                "session_time_left" => myutils_http:request_read_querystring(Req0, <<"session-time-left">>),
                "uptime" => myutils_http:request_read_querystring(Req0, uptime),
                "refresh_timeout" => myutils_http:request_read_querystring(Req0, <<"refresh-timeout">>),
                "link_status" => myutils_http:request_read_querystring(Req0, <<"link-status">>)
            },

            Tpl0 = bbmustache:parse_file(code:priv_dir(erl_app_oprex1) ++ "/webpage_templates/login-auto-submit.html"),
            TplCompile0 = bbmustache:compile(Tpl0, Params),

            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, TplCompile0, Req0),
            {ok, Req, State}
    end.

terminate(_A, _B, _C) -> ok.

get_auth_device_token(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(<<"wiqu_auth_device_token">>, 1, Cookies) of
        {_, AuthDeviceToken} -> AuthDeviceToken;
        false -> nil
    end.
