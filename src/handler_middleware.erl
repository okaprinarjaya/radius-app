-module(handler_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req0, Env) ->
    ExcludePathList = [<<"/_create-jwt">>],
    IsMember = lists:member(cowboy_req:path(Req0), ExcludePathList),
    
    if 
        IsMember =:= true ->
            {ok, Req0, Env};
        true ->
            case cowboy_req:header(<<"authorization">>, Req0, undefined) of
                undefined ->
                    {stop, myutils_http:response_unauthorized(Req0, undefined, undefined)};
                Value ->
                    case string:split(Value, " ") of
                        [_Bearer, Token] ->
                            {ok, SecretKey} = application:get_env(erl_app_oprex1, jwt_secret_key),
                            {ok, Decoded} = jwt:decode(Token, SecretKey),
                            io:format("~p~n", [Decoded]),
                            {ok, Req0, Env};
                        [_] ->
                            {stop, myutils_http:response_unauthorized(Req0, undefined, undefined)};
                        [[],[]] ->
                            {stop, myutils_http:response_unauthorized(Req0, undefined, undefined)}
                    end
            end
    end.
