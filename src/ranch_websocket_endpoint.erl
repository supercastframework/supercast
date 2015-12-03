% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Based on the work from Serge Aleynikov <saleyn at gmail.com> on the article
% www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
%
% Enms is a Network Management System aimed to manage and monitor SNMP
% targets, monitor network hosts and services, provide a consistent
% documentation system and tools to help network professionals
% to have a wide perspective of the networks they manage.
%
% Enms is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% Enms is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with Enms.  If not, see <http://www.gnu.org/licenses/>.
%%% @private
-module(ranch_websocket_endpoint).
-behaviour(cowboy_websocket_handler).
-include("supercast.hrl").
-include("logs.hrl").

-export([init/3]).
% cowboy_ewebsocket_handler
-export([websocket_init/3,websocket_handle/3,
    websocket_info/3,websocket_terminate/3]).
% supercast
-export([auth_set/2,auth_set/5,send/2,raw_send/2]).

-define(ENCODER, supercast_encoder_json).

%% @spec auth_set(success, #client_state{}, Name, Roles, AllowedMods) -> ok
%% @doc
%% Set the client authentication tokens
%% @end
auth_set(success, #client_state{pid=Pid, ref=Ref},
        Name, Roles, AllowedMods) ->
    erlang:send(Pid, {auth_success, Ref, Name, Roles, AllowedMods}).

%% @spec auth_set(success, #client_state{}) -> ok
%% @doc
%% Inform client of authentication failure
%% @end
auth_set(auth_fail, #client_state{pid=Pid, ref=Ref, user_name=UserName}) ->
    erlang:send(Pid, {auth_fail, Ref, UserName}).

%% @spec send(#client_state{}, {pdu, Message}) -> ok
%% @doc
%% Send a message to the client
%% @end
send(#client_state{pid=Pid, ref=Ref}, Message) ->
    erlang:send(Pid, {encode_send, Ref, Message}).

%% @spec send(#client_state{}, {pdu, Message}) -> ok
%% @doc
%% Send a pdu to the client
%% @end
raw_send(#client_state{pid=Pid, ref=Ref}, Pdu) ->
    erlang:send(Pid, {send, Ref, Pdu}).

init({tcp, http}, _Req, _Opts) ->
    ?LOG_INFO("init http"),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    ?LOG_INFO("init websocket"),
    State = #client_state{
        pid           = self(),
        ref           = make_ref(),
        module        = ?MODULE,
        encoding_mod  = ?ENCODER,
        authenticated = false},
    supercast_server:client_msg(connect, State),
	{ok, Req, State}.

websocket_handle({text, Pdu}, Req, State) ->
    ?LOG_INFO("data reivceved", {Pdu, State}),
    supercast_server:client_msg({message, ?ENCODER:decode(Pdu)}, State),
	{ok, Req, State};
websocket_handle(_Data, Req, State) ->
    ?LOG_INFO("Unknown handle", {_Data,Req,State}),
	{ok, Req, State}.

websocket_info({send, Ref, Pdu}, Req, #client_state{ref=Ref} = State) ->
    ?LOG_INFO(" send", {Pdu, State}),
    {reply, {text, Pdu}, Req, State};
websocket_info({encode_send, Ref, Msg},
        Req, #client_state{ref=Ref} = State) ->
    ?LOG_INFO("encode send", {Msg, State}),
    Pdu = ?ENCODER:encode(Msg),
    {reply, {text, Pdu}, Req, State};
websocket_info({auth_success, Ref, Name, Roles, Mods},
        Req, #client_state{ref=Ref} = State) ->
    ?LOG_INFO("auth success", {Name, State}),
    NextState = State#client_state{
        user_name = Name,
        user_roles = Roles,
        user_modules = Mods,
        authenticated = true},
    {ok, Req, NextState};
websocket_info({auth_fail, Ref, _UserName},
        Req, #client_state{ref=Ref} = State) ->
    ?LOG_INFO("Failed to register user", _UserName),
    {ok, Req, State};
websocket_info(_Info, Req, State) ->
    ?LOG_INFO("Unknown info", {_Info,Req,State}),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
    supercast_server:client_msg(disconnect, State),
    ?LOG_INFO("Terminated", {_Reason,_Req,State}),
	ok.

