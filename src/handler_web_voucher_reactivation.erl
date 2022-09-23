-module(handler_web_voucher_reactivation).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

-define(THREE_MONTHS_IN_SECONDS, 7890000).

init(Req0, State) ->
    ReactivationToken = myutils_http:request_read_querystring(Req0, <<"id">>),
    
    if
        ReactivationToken =/= nil ->
            Sql = <<"SELECT * FROM voucher_reactivations WHERE reactivation_token = ? AND logged_in_at IS NULL">>,
            {ok, _Cols, Rows} = mysql_poolboy:query(pool1, Sql, [ReactivationToken]),

            if
                length(Rows) > 0 ->
                    [Row|_T] = Rows,
                    Id = lists:nth(1, Row),
                    NewAuthDeviceToken = lists:nth(4, Row),
                    ReactivatedAt = lists:nth(5, Row),

                    if
                        ReactivatedAt =:= null ->
                            Sql1 = <<"UPDATE voucher_reactivations SET reactivated_at = current_timestamp() WHERE id = ?">>,
                            
                            {atomic, ok} = mysql_poolboy:transaction(pool1, fun (Pid) ->
                                mysql:query(Pid, Sql1, [Id]),
                                ok
                            end),
                            
                            Req1 = cowboy_req:set_resp_cookie(<<"wiqu_auth_device_token">>, NewAuthDeviceToken, Req0,  #{max_age => ?THREE_MONTHS_IN_SECONDS}),
                            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, <<"Voucher reactivated">>, Req1),
                            {ok, Req2, State};

                        true ->
                            Req = cowboy_req:reply(302, #{<<"location">> => <<"http://localhost:8080/login">>}, Req0),
                            {ok, Req, State}
                    end;

                true ->
                    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, <<"Invalid token">>, Req0),
                    {ok, Req, State}
            end;
        true ->
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, <<"Invalid token">>, Req0),
            {ok, Req, State}
    end.

terminate(_A, _B, _C) -> ok.
