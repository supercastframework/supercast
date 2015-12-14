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

%% @doc Dummy encoder/decoder.
-module(supercast_encoder_native).
-behaviour(supercast_encoder).

-export([encode/1, decode/1]).

-spec encode(Message::tuple()) -> Message::tuple().
encode(Message) ->
    Message.

-spec decode(Pdu::tuple()) -> Pdu::tuple().
decode(Pdu) ->
    Pdu.
