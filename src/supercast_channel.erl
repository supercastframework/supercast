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

%% @doc Supercast channel behaviour.
-module(supercast_channel).
-include("supercast.hrl").

-callback sync(Channel::string(), Args::any(), CState::#client_state{}) ->
        ok | {ok, Messages::[supercast_msg()]} | {error, Reason::term()}.
%% @doc Client join request.
%%
%% This call is triggered when a client has requested and is allowed to
%% join to the channel.
%%
%% For client synchronization purpose, the callback can return any
%% number of Pdus.
%%
%% Must call supercast:sync_ack/2 before the end of the call.
%%
%% <em>Args<em> is the term set at supercast:create/4.
%% @end

-callback leave(Channel::string, Args::any(), CState::#client_state{}) ->
        ok.
%% @doc Called when a client leave the channel.

