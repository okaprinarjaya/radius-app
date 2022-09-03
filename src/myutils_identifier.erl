-module(myutils_identifier).

-export([generate_voucher_code/2, generate_salt/0]).

generate_voucher_code(Salt, SiteId) ->
    TimeStr = integer_to_list(erlang:monotonic_time(nanosecond)),
    TimeStrLen = string:length(TimeStr),
    Digit = string:slice(TimeStr, TimeStrLen - 8, TimeStrLen),

    Ctx = hashids:new([{salt, Salt}, {min_hash_length, 8}]),
    Encoded = hashids:encode(Ctx, list_to_integer(Digit)),

    {{Year,Month,Day}, _} = calendar:local_time(),
    lists:concat([
        integer_to_list(Day),
        integer_to_list(Month),
        string:slice(integer_to_list(Year), 2, 4),
        SiteId,
        string:uppercase(Encoded)
    ]).

generate_salt() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(16))).
