%% -----------------------------------------------------------------------------
%% Supercast Copyright (c) 2012-2016
%% Sebastien Serre <ssbx@sysmo.io> All Rights Reserved.
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
%% -----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% @private
%%% @author Sebastien Serre <ssbx@sysmo.io>
%%% @copyright (C) 2015, Sebastien Serre
%%% @doc
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(supercast_channel_sup).
-behaviour(supervisor).
-include("supercast.hrl").

%% API
-export([
    start_link/0,
    new_channel/1]).

%% Supervisor callback
-export([init/1]).

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec(new_channel(Args :: [term()]) ->
    {ok, Child :: supervisor:child()} |
    {ok, Child :: supervisor:child(), Info :: term()} |
    {error,
        already_present |
        {already_started, Child :: supervisor:child()} |
        term()}).
new_channel(Args) ->
    supervisor:start_child(?MODULE, Args).

-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {
        RestartStrategy :: supervisor:strategy(),
        MaxR            :: non_neg_integer(),
        MaxT            :: non_neg_integer()},
        ChildSpecs      :: [supervisor:child_spec()]
    }} | ignore | {error, Reason :: term()}).
init([]) ->
    {ok,
        {
            {simple_one_for_one, 600, 6000},
            [
                {
                    supercast_channel,
                    {supercast_channel, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [supercast_channel]
                }
            ]
        }
    }.

