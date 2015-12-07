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

-module(supercast_endpoint).
-include("supercast.hrl").

-export([handle_message/2,init_pdu/0,client_disconnected/1]).

%% for spawn
-export([handle_client_message/2]).

%% @spec handle_message(Json::term(), Client::#client_state{}) -> ok
%% @doc
%% Handle client messages.
%% @end
handle_message(Json, Client) ->
    erlang:spawn(?MODULE, handle_client_message, [Json, Client]).

%% @private
handle_client_message(Json, Client) ->
    From = binary_to_list(proplists:get_value(<<"from">>, Json)),
    Type = binary_to_list(proplists:get_value(<<"type">>, Json)),
    handle_client_message(From, Type, Json, Client).
handle_client_message("supercast", "authResp", Contents, ClientState) ->
    Values = proplists:get_value(<<"value">>, Contents),
    Name   = binary_to_list(proplists:get_value(<<"name">>,     Values)),
    Pass   = binary_to_list(proplists:get_value(<<"password">>, Values)),
    CMod   = ClientState#client_state.module,
    AuthMod = get_env(auth_module),
    case AuthMod:authenticate(Name, Pass) of
        {ok, Groups} ->
            {ok, MainChans} = application:get_env(supercast, main_channels),
            CMod:auth_set(success, ClientState, Name, Groups, MainChans),
            send_pdu(ClientState,pdu(authAck, {Groups, MainChans}));
        fail ->
            send_pdu(ClientState, pdu(authErr, {Name, Pass}))
    end;
handle_client_message("supercast", "subscribe", Contents, ClientState) ->
    Values = proplists:get_value(<<"value">>, Contents),
    QueryId = proplists:get_value(<<"queryId">>, Values),
    Channel =  binary_to_list(proplists:get_value(<<"channel">>, Values)),
    case supercast_registrar:whereis_name(Channel) of
        undefined ->
            ?SUPERCAST_LOG_ERROR("Unknown chan name", Channel),
            send_pdu(ClientState, pdu(subscribeErr, {QueryId, Channel}));
        _ ->
            case supercast_mpd:subscribe_stage1(Channel, ClientState) of
                error ->
                    send_pdu(ClientState, pdu(subscribeErr, {QueryId, Channel}));
                ok ->
                    send_pdu(ClientState, pdu(subscribeOk, {QueryId, Channel})),
                    supercast_mpd:subscribe_stage2(Channel,ClientState)
            end
    end;
handle_client_message("supercast", "unsubscribe", Contents, ClientState) ->
    Values = proplists:get_value(<<"value">>, Contents),
    QueryId = proplists:get_value(<<"queryId">>, Values),
    Channel =  binary_to_list(proplists:get_value(<<"channel">>, Values)),
    ok = supercast_mpd:unsubscribe(Channel, ClientState),
    send_pdu(ClientState, pdu(unsubscribeOk, {QueryId, Channel}));
handle_client_message(OtherMod, Type, Contents, ClientState) ->
    handle_other_control(OtherMod, {Type, Contents}, ClientState).

%% @private
handle_other_control(ModKey, Msg, ClientState) ->
    Dispatch = get_env(pdu_dispatch),
    case lists:keyfind(ModKey, 2, Dispatch) of
        false -> {error, no_such_controler};
        {Mod, ModKey} ->
            Mod:handle_command(Msg, ClientState)
    end.

%% @spec init_pdu() -> {ok, term()}
%% @doc Return an Pdu to send to the client containing initialisation data.
init_pdu() ->
    pdu(serverInfo, {"local", get_env(http_port), "http"}).

%% @spec client_disconnected(#client_state{}) -> ok
%% @doc Must be send by the endpoint when the connexion is closed.
client_disconnected(ClientState) ->
    supercast_mpd:client_disconnect(ClientState).


%% @spec pdu(Type, Any) -> term()
%%  Type = atom()
%%  Any = term()
%% @doc Return a pdu of type Type.
pdu(serverInfo, {AuthType, DataPort, DataProto}) ->
    [
        {<<"from">>, <<"supercast">>},
        {<<"type">>, <<"serverInfo">>},
        {<<"value">>, [
            {<<"dataPort">>,  DataPort},
            {<<"dataProto">>, list_to_binary(DataProto)},
            {<<"authType">>,  list_to_binary(AuthType)}]
        }
    ];

pdu(authAck, {Groups, StaticChans}) ->
    BinGroups       = [list_to_binary(G) || G <- Groups],
    BinStaticChans  = [atom_to_binary(G, utf8) || G <- StaticChans],
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"authAck">>},
            {<<"value">>, [
                {<<"groups">>,      BinGroups},
                {<<"staticChans">>, BinStaticChans}]
            }
        ];

pdu(authErr, {Name, Password}) ->
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"authErr">>},
            {<<"value">>, [
                {<<"error">>, <<"Bad password">>},
                {<<"name">>, list_to_binary(Name)},
                {<<"password">>, list_to_binary(Password)}]
            }
        ];

pdu(subscribeOk, {QueryId, Channel}) ->
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"subscribeOk">>},
            {<<"value">>, [
                {<<"queryId">>, QueryId},
                {<<"channel">>, list_to_binary(Channel)}]
            }
        ];

pdu(subscribeErr, {QueryId, Channel}) ->
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"subscribeErr">>},
            {<<"value">>, [
                {<<"queryId">>, QueryId},
                {<<"channel">>, list_to_binary(Channel)}]
            }
        ];

pdu(unsubscribeOk, {QueryId, Channel}) ->
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"unsubscribeOk">>},
            {<<"value">>, [
                {<<"queryId">>, QueryId},
                {<<"channel">>, list_to_binary(Channel)}]
            }
        ];

pdu(unsubscribeErr, {QueryId, Channel}) ->
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"unsubscribeOk">>},
            {<<"value">>, [
                {<<"queryId">>, QueryId},
                {<<"channel">>, list_to_binary(Channel)}]
            }
        ].

get_env(Key) -> {ok, Val} = application:get_env(supercast, Key), Val.
send_pdu(#client_state{module=Mod} = CS, Msg) -> Mod:send(CS, Msg).
