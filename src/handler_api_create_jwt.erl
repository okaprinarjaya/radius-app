-module(handler_api_create_jwt).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    method_handler(Method, Req0, State).

terminate(_A, _B, _C) -> ok.

method_handler(<<"POST">>, Req0, State) ->
    Payload = [{userId, "abc123"}, {userName, "Oka Prinarjaya"}, {role, "OPERATOR"}],
    Token = myutils_identifier:generate_jwt(Payload),

    {ok, myutils_http:response_ok(Req0, Token, undefined), State};
method_handler(_, Req0, State) ->
    {ok, myutils_http:response_notfound(Req0, undefined, <<"Route not found">>), State}.
