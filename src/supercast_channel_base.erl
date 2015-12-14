%% -----------------------------------------------------------------------------
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
%% -----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% @author Sebastien Serre <ssbx@supercastframework.org>
%%% @copyright (C) 2015, Sebastien Serre
%%% @doc
%%% Supercast channel base behaviour. This behaviour is for users who do not
%%% want their channel supervised by supercast.
%%%
%%% See <em>supercast_channel.erl</em> for an example implementation.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(supercast_channel_base).
-include("supercast.hrl").

%%------------------------------------------------------------------------------
%% @see supercast:join_accept/2
%% @see supercast:join_refuse/1
%% @doc
%% Client Syn request.
%%
%% This call is triggered when a client has requested and is allowed to
%% join to the channel.
%%
%% <em>Args<em> is the term set at supercast:create/4.
%%
%% A call to this function MUST include a supercast:join_accept/1-2 or
%% supercast:join_refuse/1.
%%
%% The return value of the function is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-callback join(
    Channel :: string(),
    Args    ::any(),
    CState  ::#client_state{},
    Ref     :: {Chan :: string(), CState :: #client_state{}, QueryId::integer}
) -> any().

%%------------------------------------------------------------------------------
%% @doc
%% Called when a client leave the channel.
%%
%% Must include a call to supercast:leave_accept/1 or leave_accept/2.
%%
%% The return value of the function is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-callback leave(Channel::string, Args::any(), CState::#client_state{}) -> any().

