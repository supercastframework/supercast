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
-module(supercast_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {
            {one_for_all, 0, 6000},
            [
                {
                    supercast_channel_sup,
                    {supercast_channel_sup,start_link, []},
                    permanent,
                    2000,
                    supervisor,
                    [supercast_channel_sup]
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

