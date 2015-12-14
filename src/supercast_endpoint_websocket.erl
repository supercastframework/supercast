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

%% @private
%% @doc Websocket client implementation.
%% @TODO maybe use ninenines/bullet
-module(supercast_endpoint_websocket).
-behaviour(cowboy_websocket_handler).
-include("supercast.hrl").


-export([init/3]).
% cowboy_ewebsocket_handler
-export([websocket_init/3,websocket_handle/3,
    websocket_info/3,websocket_terminate/3]).
% supercast
-export([auth_success/3,send/2,raw_send/2]).

-spec auth_success(#client_state{}, Name::string(), Roles::[string()]) -> ok.
%% @doc Set the client authentication tokens
auth_success(#client_state{pid=Pid, ref=Ref}, Name, Roles) ->
    erlang:send(Pid, {auth_success, Ref, Name, Roles}).

-spec send(#client_state{}, {pdu, Message::supercast:sc_message()}) -> ok.
%% @doc Send a message to the client
send(#client_state{pid=Pid, ref=Ref}, Message) ->
    erlang:send(Pid, {encode_send, Ref, Message}).

-spec raw_send(#client_state{}, {pdu, Message::binary()}) -> ok.
%% @doc Send a pdu to the client
raw_send(#client_state{pid=Pid, ref=Ref}, Pdu) ->
    erlang:send(Pid, {send, Ref, Pdu}).

%% @private
init({tcp, http}, _Req, _Opts) ->
    ?SUPERCAST_LOG_INFO("init http"),
	{upgrade, protocol, cowboy_websocket}.

%% @private
websocket_init(_TransportName, Req, _Opts) ->
    ?SUPERCAST_LOG_INFO("init websocket"),
    State = #client_state{
        pid           = self(),
        ref           = make_ref(),
        module        = ?MODULE,
        authenticated = false},
    ?MODULE:send(State, supercast_endpoint:init_pdu()),
	{ok, Req, State}.

%% @private
websocket_handle({text, Pdu}, Req, State) ->
    ?SUPERCAST_LOG_INFO("data reivceved", {Pdu, State}),
    supercast_endpoint:handle_message(?ENCODER:decode(Pdu), State),
	{ok, Req, State};
websocket_handle(_Data, Req, State) ->
    ?SUPERCAST_LOG_INFO("Unknown handle", {_Data,Req,State}),
	{ok, Req, State}.

%% @private
websocket_info({send, Ref, Pdu}, Req, #client_state{ref=Ref} = State) ->
    ?SUPERCAST_LOG_INFO(" send", {Pdu, State}),
    {reply, {text, Pdu}, Req, State};
websocket_info({encode_send, Ref, Msg},
        Req, #client_state{ref=Ref} = State) ->
    ?SUPERCAST_LOG_INFO("encode send", {Msg, State}),
    Pdu = ?ENCODER:encode(Msg),
    {reply, {text, Pdu}, Req, State};
websocket_info({auth_success, Ref, Name, Roles},
        Req, #client_state{ref=Ref} = State) ->
    ?SUPERCAST_LOG_INFO("auth success", {Name, State}),
    NextState = State#client_state{
        user_name = Name,
        user_roles = Roles,
        authenticated = true},
    {ok, Req, NextState};
websocket_info(_Info, Req, State) ->
    ?SUPERCAST_LOG_INFO("Unknown info", {_Info,Req,State}),
	{ok, Req, State}.

%% @private
websocket_terminate(_Reason, _Req, State) ->
    supercast_endpoint:client_disconnected(State),
    ?SUPERCAST_LOG_INFO("Terminated", {_Reason,_Req,State}),
	ok.
