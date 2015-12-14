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

%% @doc Shared functions used by supercast_endpoint_tcp|websocket|...
-module(supercast_endpoint).
-include("supercast.hrl").

-export([handle_message/2,init_pdu/0,client_disconnected/1, pdu/2]).

%% for spawn
-export([handle_other_control/3]).


-spec handle_message(Json::term(), Client::#client_state{}) -> ok.
%% @doc Handle a client messages.
%% @private
handle_message(Json, Client) ->

    From = prop_str_val(<<"from">>, Json),
    Type = prop_str_val(<<"type">>, Json),

    handle_message(From, Type, Json, Client).


handle_message("supercast", "authResp", Contents, CState) ->

    ?SUPERCAST_LOG_INFO("handle authresp", Contents),
    Values  = prop_val(<<"value">>, Contents),
    Name    = prop_str_val(<<"name">>, Values),
    Pass    = prop_str_val(<<"password">>, Values),
    CMod    = CState#client_state.module,
    AuthMod = get_env(auth_module),

    case AuthMod:authenticate(Name, Pass) of
        {ok, Groups} ->
            MainChans = get_env(main_channels),
            CMod:auth_success(CState, Name, Groups),
            send_pdu(CState,pdu(authAck, {Groups, MainChans}));
        fail ->
            send_pdu(CState, pdu(authErr, {Name, Pass}))
    end;

handle_message(_, _, _, #client_state{authenticated = false} = CState) ->
    send_pdu(CState, pdu(authenticationRequired, []));

handle_message("supercast", "subscribe", Contents, CState) ->

    ?SUPERCAST_LOG_INFO("handle subscribe", Contents),

    Values  = prop_val(<<"value">>, Contents),
    QueryId = prop_val(<<"queryId">>, Values),
    Channel = prop_str_val(<<"channel">>, Values),

    case supercast_relay:subscribe(CState, Channel, QueryId) of
        error -> send_pdu(CState, pdu(subscribeErr, {QueryId, Channel}));
        _     -> ok
    end;

handle_message("supercast", "unsubscribe", Contents, CState) ->

    ?SUPERCAST_LOG_INFO("handle unsubscribe", Contents),

    Values  = prop_val(<<"value">>, Contents),
    QueryId = prop_val(<<"queryId">>, Values),
    Channel = prop_str_val(<<"channel">>, Values),

    ok = supercast_relay:unsubscribe(Channel, CState),
    ?SUPERCAST_LOG_INFO("after relay usubscribe"),
    send_pdu(CState, pdu(unsubscribeOk, {QueryId, Channel}));

handle_message(OtherMod, Type, Contents, CState) ->
    erlang:spawn(?MODULE, handle_other_control, [OtherMod, {Type,Contents}, CState]),
    ok.


%% @private
handle_other_control(ModKey, Msg, CState) ->
    Dispatch = get_env(pdu_dispatch),
    case lists:keyfind(ModKey, 2, Dispatch) of
        false -> {error, no_such_controler};
        {Mod, ModKey} ->
            Mod:handle_command(Msg, CState)
    end.



-spec init_pdu() -> {ok, term()}.
%% @doc Return an Pdu to send to the client containing initialisation data.
%% Called from supercast_endpoint_* modules.
%% @end
init_pdu() ->
    pdu(serverInfo, {"local", get_env(http_port), "http"}).


-spec client_disconnected(#client_state{}) -> ok.
%% @doc Must be send by the endpoint when the connexion is closed.
%% Called from supercast_endpoint_* modules.
%% @end
client_disconnected(CState) ->
    supercast_relay:unsubscribe(CState).


-spec pdu(Type::atom(), Any::term()) -> term().
%% @private
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

pdu(authenticationRequired, _) ->
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"authenticationRequired">>},
            {<<"value">>, [{}]}
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

prop_str_val(Key, Contents) -> binary_to_list(prop_val(Key, Contents)).
prop_val(Key, Contents) -> proplists:get_value(Key, Contents).
