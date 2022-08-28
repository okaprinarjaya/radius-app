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
                            case jwt:decode(Token, SecretKey) of
                                {ok, Decoded} ->
                                    {ok, Req0#{user_auth_info => Decoded}, maps:put(user_auth_info, Decoded, Env)};
                                {error,expired} ->
                                    {stop, myutils_http:response_unauthorized(Req0, undefined, <<"Token expired">>)}
                            end;
                        [_] ->
                            {stop, myutils_http:response_unauthorized(Req0, undefined, undefined)};
                        [[],[]] ->
                            {stop, myutils_http:response_unauthorized(Req0, undefined, undefined)}
                    end
            end
    end.
