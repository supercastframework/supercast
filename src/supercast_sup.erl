%% -------------------------------------------------------------------
%% Supercast Copyright (c) 2012-2015
%% Sebastien Serre <ssbx@supercastframework.org> All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%% -------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% @author Sebastien Serre <ssbx@supercastframework.org>
%%% @copyright (C) 2015, Sebastien Serre
%%% @private
%%%-----------------------------------------------------------------------------
-module(supercast_sup).
-behaviour(supervisor).
-include("supercast.hrl").

%% API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

%%------------------------------------------------------------------------------
%% @private
%% @see supervisor:start_link/3
%%------------------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @private
%% @see supervisor:start_link/3
%%------------------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok,
        {SupFlags :: {
            RestartStrategy :: supervisor:strategy(),
            MaxR            :: non_neg_integer(),
            MaxT            :: non_neg_integer()},
            [ChildSpec :: supervisor:child_spec()]
        }
    } | ignore | {error, Reason :: term()}).
init([]) ->
    {ok,
        {
            {one_for_all, 0, 6000},
            [
                {
                    supercast,
                    {supercast,start_link, []},
                    permanent,
                    2000,
                    worker,
                    [supercast]
                },
                {
                    supercast_relay_sup,
                    {supercast_relay_sup,start_link, []},
                    permanent,
                    2000,
                    supervisor,
                    [supercast_relay_sup]
                },
                {
                    ranch_sup,
                    {ranch_sup, start_link,[]},
                    permanent,
                    2000,
                    supervisor,
                    [ranch_sup]
                },
                {
                    cowboy_sup,
                    {cowboy_sup, start_link, []},
                    permanent,
                    2000,
                    supervisor,
                    [cowboy_sup]
                }
            ]
        }
    }.
