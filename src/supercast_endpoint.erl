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

%% @doc Common functions for supercast_endpoint_tcp|websocket|...
-module(supercast_endpoint).
-include("supercast.hrl").

-export([handle_message/2,init_pdu/0,client_disconnected/1, pdu/2]).

%% for spawn
-export([handle_other_control/3]).


-spec handle_message(Json :: supercast:sc_message(), Client::#client_state{}) -> ok.
%% @doc Handle a client messages.
%% @private
handle_message(Json, Client) ->

    From = prop_str_val(<<"from">>, Json),
    Type = prop_str_val(<<"type">>, Json),

    handle_message(From, Type, Json, Client).


handle_message("supercast", "authResp", Contents, CState) ->

    ?traceInfo("handle authresp", Contents),
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

    ?traceInfo("handle subscribe", Contents),

    Values  = prop_val(<<"value">>, Contents),
    QueryId = prop_val(<<"queryId">>, Values),
    Channel = prop_str_val(<<"channel">>, Values),

    case ets:lookup(?ETS_CHAN_STATES, Channel) of

        [] ->
            ?traceInfo("handle dont exists"),
            %% @TODO dynamic channels
            send_pdu(CState, pdu(subscribeErr, {QueryId, Channel}));

        [#chan_state{module=ChanMod,perm=Perm,args=Args}] ->
            ?traceInfo("exist", ChanMod),

            %% User have access right?
            {ok, Acctrl} = application:get_env(supercast,acctrl_module),
            ?traceInfo("acctrl", Acctrl),
            case Acctrl:satisfy(read, [CState], Perm) of

                {ok, []} -> %% no
                    ?traceInfo("n satisfy"),
                    send_pdu(CState, pdu(subscribeErr, {QueryId, Channel}));

                {ok, [CState]} -> % yes
                    ?traceInfo("satisfy"),

                    %% Then register
                    Ref = {Channel, CState, QueryId},
                    ChanMod:join_request(Channel, Args, CState, Ref)
            end;
        _Other ->
            ?traceInfo("other", _Other)
    end;


handle_message("supercast", "unsubscribe", Contents, CState) ->

    ?traceInfo("handle unsubscribe", Contents),

    Values  = prop_val(<<"value">>, Contents),
    QueryId = prop_val(<<"queryId">>, Values),
    Channel = prop_str_val(<<"channel">>, Values),

    case ets:lookup(?ETS_CHAN_STATES, Channel) of
        [] ->
            ?traceInfo("handle unsubscribe false"),
            send_pdu(CState, pdu(unsubscribeErr, {QueryId, Channel}));
        [#chan_state{name=Name,module=Mod,args=Args}] ->
            ?traceInfo("handle unsubscribe true"),
            Mod:leave_request(Name, Args, CState, {Name, CState, QueryId});
        _Other ->
            ?traceInfo("handle unsubscribe other", _Other)

    end;


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



-spec init_pdu() -> {ok, InitPdu :: supercast:sc_message()}.
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
    lists:foreach(fun(#chan_state{module=Mod,args=Args,name=Name}) ->
        Mod:leave_request(Name,Args, CState, {Name, CState, undefined})
    end, ets:tab2list(?ETS_CHAN_STATES)).


-spec pdu(Type :: atom(), Any :: term()) -> supercast:sc_message().
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
            {<<"type">>, <<"unsubscribeErr">>},
            {<<"value">>, [
                {<<"queryId">>, QueryId},
                {<<"channel">>, list_to_binary(Channel)}]
            }
        ].

get_env(Key) -> {ok, Val} = application:get_env(supercast, Key), Val.
send_pdu(#client_state{module=Mod} = CS, Msg) -> Mod:send(CS, Msg).

prop_str_val(Key, Contents) -> binary_to_list(prop_val(Key, Contents)).
prop_val(Key, Contents) -> proplists:get_value(Key, Contents).
