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
%%% @author Sebastien Serre <ssbx@sysmo.io>
%%% @copyright (C) 2015, Sebastien Serre
%%% @doc
%%% Controler behaviour. This module is defined as receiver of clients
%%% calls in the sys.config file 'pdu_dispatch' section.
%%% Note that the call to handle_command is not done by a spawned process, not
%%% by the client endpoint.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(supercast_controller).
-include("supercast.hrl").

-callback handle_command(Msg::tuple(), CState::#client_state{}) -> ok.

