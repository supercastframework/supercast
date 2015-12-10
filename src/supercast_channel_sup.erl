
-module(supercast_channel_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,start_child/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

init([]) ->
    {ok,
        {
            {simple_one_for_one, 0, 6000},
            [
                {
                    supercast_relay,
                    {supercast_relay,start_link,[]},
                    transiant,
                    2000,
                    worker,
                    [supercast_relay]
                }
            ]
        }
    }.

