-module(erl_app_oprex1_app).

-behaviour(application).

-export([start/2, stop/1, test_hashids/0]).

start(_StartType, _StartArgs) ->
    Host_Paths = [
        {"/", hello_handler, []},
        {"/assets/[...]", cowboy_static, {priv_dir, erl_app_oprex1, "webpage_assets"}}
    ],
    Host = {'_', Host_Paths},
    
    Dispatch = cowboy_router:compile([Host]),
    {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    
    erl_app_oprex1_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
test_hashids() ->
    TimeStr = integer_to_list(erlang:monotonic_time(nanosecond)),
    TimeStrLen = string:length(TimeStr),
    Digit = string:slice(TimeStr, TimeStrLen - 8, TimeStrLen),
    Salt = binary_to_list(base64:encode(crypto:strong_rand_bytes(16))),

    Ctx = hashids:new([{salt, Salt}, {min_hash_length, 8}]),
    Encoded = hashids:encode(Ctx, list_to_integer(Digit)),

    {{Year,Month,Day}, _} = calendar:local_time(),
    VoucherNumberStr = lists:concat([
        integer_to_list(Day),
        integer_to_list(Month),
        string:slice(integer_to_list(Year), 2, 4),
        "1",
        string:uppercase(Encoded)
    ]),

    io:format("ID: ~s~n", [VoucherNumberStr]).
