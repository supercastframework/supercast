% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
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
% @private
-module(supercast_server).
-behaviour(gen_server).
-include("include/supercast.hrl").
-include_lib("common_hrl/include/logs.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/0,
    client_msg/2
]).

-record(state, {
    auth_mod,
    dispatch,
    http_port,
    http_proto
}).


%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


which_auth() ->
    gen_server:call(?MODULE, {get, auth_mod}).



% API FROM CLIENTS
client_msg(connect, ClientState) ->
    handle_client_msg(connect, ClientState);
client_msg(disconnect, ClientState) ->
    handle_client_msg(disconnect, ClientState);
client_msg({message, Json}, ClientState) ->
    {struct, Contents} = Json,

    From = binary_to_list(proplists:get_value(<<"from">>, Contents)),
    Type = binary_to_list(proplists:get_value(<<"type">>, Contents)),

    handle_client_msg({From, Type, Contents}, ClientState).

% from himself
handle_client_command(Mod, Msg, CState) ->
    gen_server:call(?MODULE, {client_command, Mod, Msg, CState}).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
init([]) ->
    {ok, AuthModule}  = application:get_env(supercast, auth_module),
    {ok, PduDispatch} = application:get_env(supercast, pdu_dispatch),
    {ok, HttpUseSSL}  = application:get_env(supercast, http_use_ssl),

    %% {ok, HttpPort}    = application:get_env(supercast, http_port),
    %% TODO use a configuration store module to avoid dependency
    %% loop sysmo <-> supercast.
    HttpPort = j_server:get_http_port(),
    ?LOG_INFO("Get http port", {port, HttpPort}),

    case HttpUseSSL of
        true ->
            DataProto = "https";
        false ->
            DataProto = "http"
    end,
    {ok, #state{
        auth_mod   = AuthModule,
        dispatch   = PduDispatch,
        http_port  = HttpPort,
        http_proto = DataProto
        }
    }.

% @private
handle_call(dump, _F, S) ->
    {reply, {ok, S}, S};

handle_call({connect, CState}, _F, #state{http_port = Port , http_proto = Proto} = S) ->
    R = send(CState, pdu(serverInfo, {"local", Port, Proto})),
    {reply, R, S};

handle_call({client_command, Key, Msg, CState}, _F, #state{dispatch = Dispatch} = S) ->
    case lists:keyfind(Key, 2, Dispatch) of
        false ->
            {reply, ok, S};
        {Module, Key} ->
            Module:handle_command(Msg, CState),
            {reply, ok, S}
    end;

handle_call({get, auth_mod}, _F, #state{auth_mod = AuthMod} = S) ->
    {reply, AuthMod, S};

% CLIENT RELATED CALLS
handle_call(_Call, _F, S) ->
    ?LOG_WARNING("Unknown call", _Call),
    {noreply, S}.

% CAST
% @private
handle_cast(_Cast, S) ->
    ?LOG_WARNING("Unknown cast", _Cast),
    {noreply, S}.

% OTHER
% @private
handle_info(_I, S) ->
    {noreply, S}.

% @private
terminate(_R, _S) ->
    normal.

% @private
code_change(_O, S, _E) ->
    {ok, S}.

% CLIENT CALL
% @private
handle_client_msg(connect, ClientState) ->
    gen_server:call(?MODULE, {connect, ClientState});

handle_client_msg(disconnect, ClientState) ->
    supercast_mpd:client_disconnect(ClientState);

handle_client_msg({"supercast", "authResp", Contents}, ClientState) ->
    {struct, Values} = proplists:get_value(<<"value">>, Contents),
    Name = binary_to_list(proplists:get_value(<<"name">>,     Values)),
    Pass = binary_to_list(proplists:get_value(<<"password">>, Values)),
    CMod = ClientState#client_state.module,
    AuthMod = which_auth(),
    case AuthMod:authenticate(Name, Pass) of
        {ok, Groups} ->
            MainChans = supercast_mpd:main_chans(),
            CMod:auth_set(success, ClientState, Name, Groups, MainChans),
            send(ClientState,pdu(authAck, {Groups, MainChans}));
        fail    ->
            send(ClientState, pdu(authErr, {Name, Pass}))
    end;

handle_client_msg({"supercast", "subscribe", Contents}, ClientState) ->
    {struct, Values} = proplists:get_value(<<"value">>, Contents),
    QueryId = proplists:get_value(<<"queryId">>, Values),
    Channel =  binary_to_list(proplists:get_value(<<"channel">>, Values)),
    case supercast_registrar:whereis_name(Channel) of
        undefined ->
            ?LOG_ERROR("Unknown chan name", Channel),
            send(ClientState, pdu(subscribeErr, {QueryId, Channel}));
        _ ->
            case supercast_mpd:subscribe_stage1(Channel, ClientState) of
                error ->
                    send(ClientState, pdu(subscribeErr, {QueryId, Channel}));
                ok ->
                    send(ClientState, pdu(subscribeOk, {QueryId, Channel})),
                    supercast_mpd:subscribe_stage2(Channel,ClientState)
            end
    end;

handle_client_msg({"supercast", "unsubscribe", Contents}, ClientState) ->
    {struct, Values} = proplists:get_value(<<"value">>, Contents),
    QueryId = proplists:get_value(<<"queryId">>, Values),
    Channel =  binary_to_list(proplists:get_value(<<"channel">>, Values)),
    ok = supercast_mpd:unsubscribe(Channel, ClientState),
    send(ClientState, pdu(unsubscribeOk, {QueryId, Channel}));

handle_client_msg({OtherMod, Type, Contents}, ClientState) ->
    handle_client_command(OtherMod, {Type, Contents}, ClientState).

% server PDUs
% @private
pdu(serverInfo, {AuthType, DataPort, DataProto}) ->
    {struct,
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"serverInfo">>},
            {<<"value">>, {struct, [
                {<<"dataPort">>,  DataPort},
                {<<"dataProto">>, list_to_binary(DataProto)},
                {<<"authType">>,  list_to_binary(AuthType)}]}
            }
        ]
    };

pdu(authAck, {Groups, StaticChans}) ->
    BinGroups       = [list_to_binary(G) || G <- Groups],
    BinStaticChans  = [atom_to_binary(G, utf8) || G <- StaticChans],
    {struct,
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"authAck">>},
            {<<"value">>, {struct, [
                {<<"groups">>, {array, BinGroups}},
                {<<"staticChans">>, {array, BinStaticChans}}]}
            }
        ]
    };

pdu(authErr, {Name, Password}) ->
    {struct,
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"authErr">>},
            {<<"value">>, {struct, [
                {<<"error">>, <<"Bad password">>},
                {<<"name">>, list_to_binary(Name)},
                {<<"password">>, list_to_binary(Password)}]}
            }
        ]
    };

pdu(subscribeOk, {QueryId, Channel}) ->
    {struct,
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"subscribeOk">>},
            {<<"value">>, {struct, [
                {<<"queryId">>, QueryId},
                {<<"channel">>, list_to_binary(Channel)}]}
            }
        ]
    };

pdu(subscribeErr, {QueryId, Channel}) ->
    {struct,
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"subscribeErr">>},
            {<<"value">>, {struct, [
                {<<"queryId">>, QueryId},
                {<<"channel">>, list_to_binary(Channel)}]}
            }
        ]
    };

pdu(unsubscribeOk, {QueryId, Channel}) ->
    {struct,
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"unsubscribeOk">>},
            {<<"value">>, {struct, [
                {<<"queryId">>, QueryId},
                {<<"channel">>, list_to_binary(Channel)}]}
            }
        ]
    };

pdu(unsubscribeErr, {QueryId, Channel}) ->
    {struct,
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"unsubscribeOk">>},
            {<<"value">>, {struct, [
                {<<"queryId">>, QueryId},
                {<<"channel">>, list_to_binary(Channel)}]}
            }
        ]
    }.

send(#client_state{module = CMod} = ClientState, Msg) ->
    CMod:send(ClientState, Msg).




