%% -------------------------------------------------------------------
%% Supercast Copyright (c) 2012-2015 Sebastien Serre <ssbx@sysmo.io>
%% All Rights Reserved.
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
-module(supercast_endpoint_websocket).
-behaviour(cowboy_websocket_handler).
-include("supercast.hrl").

%% @TODO maybe use ninenines/bullet

-export([init/3]).
% cowboy_ewebsocket_handler
-export([websocket_init/3,websocket_handle/3,
    websocket_info/3,websocket_terminate/3]).
% supercast
-export([auth_set/2,auth_set/5,send/2,raw_send/2]).

-define(ENCODER, jsx).

%% @spec auth_set(success, #client_state{}, Name, Roles, AllowedMods) -> ok
%% @doc Set the client authentication tokens
auth_set(success, #client_state{pid=Pid, ref=Ref},
        Name, Roles, AllowedMods) ->
    erlang:send(Pid, {auth_success, Ref, Name, Roles, AllowedMods}).

%% @spec auth_set(success, #client_state{}) -> ok
%% @doc Inform client of authentication failure
auth_set(auth_fail, #client_state{pid=Pid, ref=Ref, user_name=UserName}) ->
    erlang:send(Pid, {auth_fail, Ref, UserName}).

%% @spec send(#client_state{}, {pdu, Message}) -> ok
%%  Message = term()
%% @doc Send a message to the client
send(#client_state{pid=Pid, ref=Ref}, Message) ->
    erlang:send(Pid, {encode_send, Ref, Message}).

%% @spec raw_send(#client_state{}, {pdu, Message}) -> ok
%%  Message = binary()
%% @doc Send a pdu to the client
raw_send(#client_state{pid=Pid, ref=Ref}, Pdu) ->
    erlang:send(Pid, {send, Ref, Pdu}).

init({tcp, http}, _Req, _Opts) ->
    ?SUPERCAST_LOG_INFO("init http"),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    ?SUPERCAST_LOG_INFO("init websocket"),
    State = #client_state{
        pid           = self(),
        ref           = make_ref(),
        module        = ?MODULE,
        encoding_mod  = ?ENCODER,
        authenticated = false},
    ?MODULE:send(State, supercast_endpoint:init_pdu()),
	{ok, Req, State}.

websocket_handle({text, Pdu}, Req, State) ->
    ?SUPERCAST_LOG_INFO("data reivceved", {Pdu, State}),
    supercast_endpoint:handle_message(?ENCODER:decode(Pdu), State),
	{ok, Req, State};
websocket_handle(_Data, Req, State) ->
    ?SUPERCAST_LOG_INFO("Unknown handle", {_Data,Req,State}),
	{ok, Req, State}.

websocket_info({send, Ref, Pdu}, Req, #client_state{ref=Ref} = State) ->
    ?SUPERCAST_LOG_INFO(" send", {Pdu, State}),
    {reply, {text, Pdu}, Req, State};
websocket_info({encode_send, Ref, Msg},
        Req, #client_state{ref=Ref} = State) ->
    ?SUPERCAST_LOG_INFO("encode send", {Msg, State}),
    Pdu = ?ENCODER:encode(Msg),
    {reply, {text, Pdu}, Req, State};
websocket_info({auth_success, Ref, Name, Roles, Mods},
        Req, #client_state{ref=Ref} = State) ->
    ?SUPERCAST_LOG_INFO("auth success", {Name, State}),
    NextState = State#client_state{
        user_name = Name,
        user_roles = Roles,
        user_modules = Mods,
        authenticated = true},
    {ok, Req, NextState};
websocket_info({auth_fail, Ref, _UserName},
        Req, #client_state{ref=Ref} = State) ->
    ?SUPERCAST_LOG_INFO("Failed to register user", _UserName),
    {ok, Req, State};
websocket_info(_Info, Req, State) ->
    ?SUPERCAST_LOG_INFO("Unknown info", {_Info,Req,State}),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
    supercast_endpoint:client_disconnected(State),
    ?SUPERCAST_LOG_INFO("Terminated", {_Reason,_Req,State}),
	ok.

