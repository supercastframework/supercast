
-module(supercast_relay_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,start_relay/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_relay(Args) ->
    supervisor:start_child(?MODULE, Args).

init([]) ->
    {ok,
        {
            {simple_one_for_one, 100, 5000},
            [
                {
                    supercast_relay,
                    {supercast_relay,start_link,[]},
                    transient,
                    2000,
                    worker,
                    [supercast_relay]
                }
            ]
        }
    }.
