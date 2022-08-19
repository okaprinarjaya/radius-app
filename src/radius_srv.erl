-module(radius_srv).

-behaviour(gen_server).
-behaviour(eradius_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, radius_request/3, strip/1]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("include/dictionary_mikrotik.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    eradius:load_tables(code:priv_dir(erl_app_oprex1) ++ "/dictionaries", [dictionary_mikrotik]),
    eradius:modules_ready([?MODULE]),
    {ok, self()}.

radius_request(R = #radius_request{cmd = request}, _NasProp, _Args) ->
     F = fun(Attr) ->       
        case Attr of
            {{attribute,4,ipaddr,"NAS-IP-Address",no}, NAS_IP_Address} ->
                logger:info("NAS-IP-Address: ~p", [NAS_IP_Address]);
            {{attribute,1,string,"User-Name",no}, User_Name} ->
                logger:info("User-Name: ~p", [User_Name]);
            {{attribute,2,string,"User-Password",scramble}, User_Password_Bin} ->
                <<User_Password:13/binary, _:3/binary>> = User_Password_Bin,
                logger:info("User-Password: ~p", [strip(User_Password)])
        end
    end,

    [F(Attr) || Attr <- R#radius_request.attrs],
    RespAttrs = [ 
        {?Session_Timeout, 3600},
        {?Port_Limit, 1},
        {?WISPr_Session_Terminate_Time, "2022-01-01T07:00:00-07:00"},
        {?Mikrotik_Rate_Limit, "1"},
        {?Mikrotik_Group, "MyGroup"}
    ],
    Response = #radius_request{cmd = accept, attrs = RespAttrs},
    {reply, Response};
radius_request(_R = #radius_request{cmd = accreq}, _NasProp, _Args) ->
    Response = #radius_request{cmd = accresp, attrs = [{?Menu, <<"foo">>}]},
    {reply, Response}.

handle_call(_Request, _From, _State) ->
    ok.

handle_cast(_Request, _State) ->
    ok.

strip(B) ->
    strip(B, erlang:byte_size(B) - 1).
  
strip(_B, -1) ->
    <<>>;
strip(B, Idx) ->
    case binary:at(B, Idx) of
        0 -> strip(B, Idx - 1);
        _ -> binary:part(B, 0, Idx + 1)
    end.
