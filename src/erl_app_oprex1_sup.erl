-module(erl_app_oprex1_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 10
    },
    ChildSpecs = [
        #{
            id => radius_srv_id,
            start => {radius_srv, start_link, []}
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
