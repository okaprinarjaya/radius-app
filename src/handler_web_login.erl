-module(handler_web_login).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    Tpl = bbmustache:parse_file(code:priv_dir(erl_app_oprex1) ++ "/webpage_templates/main.html"),
    TplCompile = bbmustache:compile(Tpl, #{"title" => "Wiqu Login Voucher"}),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, TplCompile, Req0),
    {ok, Req, State}.

terminate(_A, _B, _C) -> ok.
