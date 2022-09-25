-module(erl_app_oprex1_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Host_Paths = [
        {"/", handler_root, []},
        {"/login", handler_web_login, []},
        {"/web/voucher-reactivation", handler_web_voucher_reactivation, []},
        {"/web/assets/[...]", cowboy_static, {priv_dir, erl_app_oprex1, "webpage_assets"}},
        {"/api/voucher-categories", handler_voucher_categories, []},
        {"/api/sites", handler_sites, []},
        {"/api/vouchers", handler_vouchers, []},
        {"/api/voucher-reactivations", handler_voucher_reactivations, []},
        {"/api/_create-jwt", handler_create_jwt, []}
    ],
    Host = {'_', Host_Paths},
    
    Dispatch = cowboy_router:compile([Host]),
    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, 8080}],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [middleware_handler, cowboy_router, cowboy_handler]
        }
    ),
    
    erl_app_oprex1_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
