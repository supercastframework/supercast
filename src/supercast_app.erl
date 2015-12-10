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

%% @private
-module(supercast_app).
-behaviour(application).
-include("supercast.hrl").

-export([start/2, stop/1]).

start(_Type, _Args) ->
    init_relay_ets(),
    init_chan_ets(),
    supercast_sup:start_link().

stop(_State) -> ok.

%% @doc Initialize ets used by supercast_reg module to store
%% supercast_relay pids.
%% @end
init_relay_ets() ->
    ets:new(?ETS_RELAYS_REGISTER, [set, public, named_table,
        {write_concurrency, false}, {read_concurrency, true}, {keypos, 1}]).

%% @doc Initialize ets used to store channel subscribers to a channel.
init_chan_ets() ->
    ets:new(?ETS_CHAN_STATES, [set, public, named_table,
        {write_concurrency, false}, {read_concurrency, true}, {keypos, 2}]).
