-module(create_jwt_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    method_handler(Method, Req0, State).

terminate(_A, _B, _C) -> ok.

method_handler(<<"POST">>, Req0, State) ->
    Payload = [{userId, "abc123"}, {userName, "Oka Prinarjaya"}, {role, "OPERATOR"}],
    {ok, SecretKey} = application:get_env(erl_app_oprex1, jwt_secret_key),
    {ok, ExpiredInSeconds} = application:get_env(erl_app_oprex1, jwt_expired_seconds),
    {ok, Token} = jwt:encode(<<"HS256">>, Payload, ExpiredInSeconds, SecretKey),

    {ok, myutils_http:response_ok(Req0, Token, undefined), State};
method_handler(_, Req0, State) ->
    {ok, myutils_http:response_notfound(Req0, undefined, <<"Route not found">>), State}.
