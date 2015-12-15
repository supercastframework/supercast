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
%%% Supercast channel behaviour.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(supercast_channel).
-include("supercast.hrl").

%% API
-export([
    new/4,
    delete/1,
    unicast/2,
    multicast/3,
    broadcast/2,
    leave_ack/1,
    leave_ack/2,
    join_accept/1,
    join_accept/2,
    join_refuse/1]).



%%%=============================================================================
%%% Behaviour callbacks definition
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @see supercast:join_accept/2
%% @see supercast:join_refuse/1
%% @doc
%% Client Syn request.
%%
%% This call is triggered when a client has requested and is allowed to
%% join to the channel.
%%
%% <em>Args<em> is the term set at supercast_channel:create/4.
%%
%% A call to this function MUST include a supercast_channel:join_accept/1-2 or
%% supercast:join_refuse/1.
%%
%% The return value of the function is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-callback join(
    Channel :: string(),
    Args    :: any(),
    CState  :: #client_state{},
    Ref     :: supercast:sc_reference()
) -> any().

%%------------------------------------------------------------------------------
%% @doc
%% Called when a client leave the channel either on socket close or
%% unsubscribe call.
%%
%% A call to this function MUST include a supercast_channel:leave_ack/1-2
%%
%% The return value of the function is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-callback leave(Channel :: string, Args :: any(),
    CState :: #client_state{}, Ref :: supercast:sc_reference()) -> any().



%%%=============================================================================
%%% User API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @see supercast_channel
%% @doc
%% Register a new channel.
%%
%% <em>ChanName</em> is the name of the channel.
%%
%% <em>Module</em> is the name of the module implementing the supercast_channel
%% behaviour.
%%
%% <em>Opts</em> is any term passed to supercast_channel callbacks.
%% @see supercast_channel:join/3
%% @see supercast_channel:leave/1
%%
%% <em>Perm</em> is the permissions of the channel. Only read is handled by
%% supercast.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(new(ChanName :: string(), Module :: atom(), Opts :: any(),
        Perm :: #perm_conf{}) -> ok).
new(Channel, Module, Args, Perm) ->
    ets:insert(?ETS_CHAN_STATES,
        #chan_state{name=Channel,module=Module,perm=Perm,args=Args}),
    ok.


%%------------------------------------------------------------------------------
%% @doc
%% Delete a registered channel.
%%
%% This function will send a <em>channelVanished</em> message to all connected
%% clients.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(delete(ChannName :: string()) -> ok).
delete(ChanName) ->
    ets:delete(?ETS_CHAN_STATES, ChanName).


%%------------------------------------------------------------------------------
%% @doc
%% Send messages directly to a client.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(unicast(CState :: #client_state{},
    Messages :: [supercast:sc_message()]) -> ok).
unicast(#client_state{module=Mod} = CState, Messages) ->
    lists:foreach(fun(M) -> Mod:send(CState,M) end, Messages).


%%------------------------------------------------------------------------------
%% @see unicast/3
%% @see multicast/2
%% @doc
%% Send messages to all clients of the specified channel, wich satisfy with the
%% <em>read</em> permission of the #perm_conf{}.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(multicast(Channel :: string(), Messages :: [supercast:sc_message()],
                                CustomPerm :: default | #perm_conf{}) -> ok).
multicast(Channel, Messages, default) ->
    case ets:lookup(?ETS_CHAN_STATES, Channel) of
        [#chan_state{clients=[]}] -> ok;
        [#chan_state{clients=Clients}] ->
            multi_send(Clients, Messages)
    end;
multicast(Channel, Messages, Perm) ->
    case ets:lookup(?ETS_CHAN_STATES, Channel) of
        [#chan_state{clients=[]}] -> ok;
        [#chan_state{clients=Clients}] ->
            {ok, Acctrl}  = application:get_env(supercast, acctrl_module),
            {ok, Allowed} = Acctrl:satisfy(read, Clients, Perm),
            multi_send(Allowed, Messages)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Helper to send multiple pdus to multipe clients.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(multi_send(Clients :: [#client_state{}],
    Messages :: [supercast:sc_message()]) -> ok).
multi_send(Clients, Msgs) ->
    lists:foreach(fun(Message) ->
        Pdu = ?ENCODER:encode(Message),
        lists:foreach(fun(#client_state{module=Mod} = Client) ->
            Mod:raw_send(Client, Pdu)
        end, Clients)
    end, Msgs).

%%------------------------------------------------------------------------------
%% @equiv multicast(Channel, Messages, default).
%% @see multicast/3
%% @doc
%% Send messages to all clients of the specified channel.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(broadcast(Channel :: string(), Message :: [supercast:sc_message()]) -> ok).
broadcast(Channel, Message) ->
    multicast(Channel, Message, default).


%%------------------------------------------------------------------------------
%% @equiv join_ack(Ref, [])..
%%------------------------------------------------------------------------------
-spec(join_accept(Ref :: supercast:sc_reference()) -> ok).
join_accept(Ref) -> join_accept(Ref, []).


%%------------------------------------------------------------------------------
%% @see join_refuse/1
%% @doc
%% Must be called from <em>supercast_channel:join/3</em> to effectively
%% subscribe the client to the channel.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(join_accept(Ref :: supercast:sc_reference(),
        Pdus :: [supercast:sc_message()]) -> ok).
join_accept({Channel, #client_state{module=Mod} = CState, QueryId}, Pdus) ->

    OkJoin = supercast_endpoint:pdu(subscribeOk, {QueryId, Channel}),

    lists:foreach(fun(P) -> Mod:send(CState, P) end, [OkJoin|Pdus]),

    [#chan_state{clients=Clients} = CS] = ets:lookup(?ETS_CHAN_STATES, Channel),
    ets:insert(?ETS_CHAN_STATES, CS#chan_state{clients=[CState|Clients]}).

%%------------------------------------------------------------------------------
%% @equiv leave_ack(Ref, [])..
%%------------------------------------------------------------------------------
-spec(leave_ack(Ref :: supercast:sc_reference()) -> ok).
leave_ack(Ref) -> leave_ack(Ref, []).


%%------------------------------------------------------------------------------
%% @doc
%% Must be called from <em>supercast_channel:leave/3</em> to effectively
%% unsubscribe the client to the channel.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(leave_ack(Ref :: supercast:sc_reference(),
    Pdus :: [supercast:sc_message()]) -> ok).
leave_ack({Channel, CState, undefined}, _Pdus) ->
    [#chan_state{clients=Clients} = CS] = ets:lookup(?ETS_CHAN_STATES, Channel),
    Clients2 = lists:delete(CState, Clients),
    ets:insert(?ETS_CHAN_STATES, CS#chan_state{clients=Clients2});
leave_ack({Channel, #client_state{module=Mod} = CState, QueryId}, Pdus) ->

    OkLeave = supercast_endpoint:pdu(unsubscribeOk, {QueryId, Channel}),

    lists:foreach(fun(P) ->
        Mod:send(CState, P)
    end, lists:append(Pdus, [OkLeave])),

    ?SUPERCAST_LOG_INFO("leave ack"),

    [#chan_state{clients=Clients} = CS] = ets:lookup(?ETS_CHAN_STATES, Channel),
    ?SUPERCAST_LOG_INFO("clients 2: ", Clients),
    Clients2 = lists:delete(CState, Clients),
    ?SUPERCAST_LOG_INFO("clients 2: ", Clients2),
    ets:insert(?ETS_CHAN_STATES, CS#chan_state{clients=Clients2}).


%%------------------------------------------------------------------------------
%% @see join_accept/2
%% @doc
%% Must be called from <em>supercast_channel:join/3</em> to cancel the user
%% request to join the channel. It notify the client with  a
%% <em>subscribeErr</em> message.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(join_refuse(Ref :: supercast:sc_reference()) -> ok).
join_refuse({Channel, #client_state{module=Mod} = CState, QueryId}) ->
    ErrPdu = supercast_endpoint:pdu(subscribeErr, {QueryId, Channel}),
    Mod:send(CState, ErrPdu).

