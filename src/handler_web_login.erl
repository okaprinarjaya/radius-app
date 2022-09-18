-module(handler_web_login).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
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
