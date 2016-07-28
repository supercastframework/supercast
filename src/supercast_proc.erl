%% -----------------------------------------------------------------------------
%% Supercast Copyright (c) 2012-2016
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
%%% This module describe the implementation of a supercast_proc module (ie
%%% supercast_channel). It is here for users who wish to extent their own
%%% application process to become supercast_proc channels.
%%%
%%% A supercast_proc module must export join_request, leave_request and
%%% info_request. All functions of this module must be called from the same
%%% process to insure a predictable message order arrival on the client side.
%%%
%%%
%%% <h4>join_request/4</h4>
%%% This call is triggered when a client has requested and is allowed to
%%% join to the channel.
%%%
%%% <em>Args</em> is the term set at supercast_proc:new_channel/4.
%%%
%%% A call to this function MUST include a supercast_proc:join_accept/1-2 or
%%% supercast_proc:join_refuse/1.
%%%
%%% A well behaving proc should eather reply as soon as possible
%%% (ie: gen_server:cast/2) or raise an exception wich will send a subscribeErr
%%% to the client (ie: the channel does not exists).
%%% See <em>supercast_channel</em> implementation.
%%%
%%% The return value of the function is ignored.
%%%
%%% <h4>leave_request/4</h4>
%%% Called when a client leave the channel either on socket close or
%%% unsubscribe call.
%%%
%%% A call to this function MUST include a supercast_proc:leave_ack/1-2
%%%
%%% A well behaving proc should reply as soon as possible ie: gen_server:cast/2
%%%
%%% See <em>supercast_channel</em> implementation.
%%%
%%% The return value of the function is ignored.
%%%
%%% <h4>info_request/4</h4>
%%% Used for communicate with the channel process .
%%%
%%% See <em>supercast_channel</em> implementation.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(supercast_proc).

-include("supercast.hrl").

-export([
    new_channel/4,
    delete_channel/1,
    info_request/2]).

-export([%% supercast_proc API
    send_unicast/2,
    send_unicast/3,
    send_multicast/3,
    send_broadcast/2,
    leave_ack/1,
    leave_ack/2,
    join_accept/1,
    join_accept/2,
    join_refuse/1]).

%%%=============================================================================
%%% Behaviour callbacks definition
%%%=============================================================================

-callback join_request(Channel::string(), Args::any(), CState::#client_state{},
            Ref::supercast:sc_reference()) -> term().

-callback leave_request(Channel::string(), Args::term(),
    CState :: #client_state{}, Ref :: supercast:sc_reference()) -> term().

-callback info_request(Channel::string(), Args::term(),
    Request :: term()) -> ok.



%%%=============================================================================
%%% API
%%%=============================================================================

info_request(Name, Request) ->
    case ets:lookup(?ETS_CHAN_STATES, Name) of
        [] -> ok;
        [#chan_state{module=Mod,args=Args}] ->
            Mod:info_request(Name, Args, Request)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Register a new channel.
%%
%% <em>ChanName</em> is the name of the channel.
%%
%% <em>Module</em> is the name of the module implementing the supercast_proc
%% behaviour.
%%
%% <em>Opts</em> is any term passed to supercast callbacks.
%% @see supercast_proc:join_ack/3.
%% @see supercast_proc:leave_ack/1.
%%
%% <em>Perm</em> is the permissions of the channel. Only read is handled by
%% supercast.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(new_channel(ChanName :: string(), Module :: atom(), Opts :: any(),
        Perm :: #perm_conf{}) -> ok).
new_channel(Channel, Module, Args, Perm) ->
    ets:insert(?ETS_CHAN_STATES,
        #chan_state{name=Channel,module=Module,perm=Perm,args=Args}),
    ok.


%%------------------------------------------------------------------------------
%% @doc
%% Delete a registered channel.
%% @TODO send a channel vanished message
%%
%% @end
%%------------------------------------------------------------------------------
-spec(delete_channel(Channel :: string()) -> ok).
delete_channel(Channel) ->
    case ets:lookup(?ETS_CHAN_STATES, Channel) of
        [] -> ok;
        [#chan_state{clients=Clients}] ->
            multi_send(Clients,
                [supercast_endpoint:pdu(channelVanished, Channel)])
    end,
    ets:delete(?ETS_CHAN_STATES, Channel).


%%------------------------------------------------------------------------------
%% @doc
%% Send messages directly to a client.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(send_unicast(CState :: #client_state{},
    Messages :: [supercast:sc_message()]) -> ok).
send_unicast(#client_state{module=Mod} = CState, Messages) ->
    lists:foreach(fun(M) -> Mod:send(CState,M) end, Messages).

%%------------------------------------------------------------------------------
%% @equiv unicast/2
%% @doc
%% Actualy equal to send_unicast/2.
%% @TODO insert Channel in pdu
%%
%% @end
%%------------------------------------------------------------------------------
-spec(send_unicast(_Channel :: string(), CState :: #client_state{},
    Messages :: [supercast:sc_message()]) -> ok).
send_unicast(_Channel, CState, Messages) ->
    send_unicast(CState, Messages).


%%------------------------------------------------------------------------------
%% @see send_unicast/3
%% @see send_multicast/2
%% @doc
%% Send messages to all clients of the specified channel, wich satisfy with the
%% <em>read</em> permission of the #perm_conf{}.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(send_multicast(Channel :: string(), Messages :: [supercast:sc_message()],
                                CustomPerm :: default | #perm_conf{}) -> ok).
send_multicast(Channel, Messages, default) ->
    case ets:lookup(?ETS_CHAN_STATES, Channel) of
        [#chan_state{clients=[]}] -> ok;
        [#chan_state{clients=Clients}] ->
            multi_send(Clients, Messages)
    end;
send_multicast(Channel, Messages, Perm) ->
    case ets:lookup(?ETS_CHAN_STATES, Channel) of
        [#chan_state{clients=[]}] -> ok;
        [#chan_state{clients=Clients}] ->
            {ok, Acctrl}  = application:get_env(supercast, acctrl_module),
            {ok, Allowed} = Acctrl:satisfy(read, Clients, Perm),
            multi_send(Allowed, Messages)
    end.


%%------------------------------------------------------------------------------
%% @equiv multicast(Channel, Messages, default)
%% @see multicast/3.
%% @doc
%% Send messages to all clients of the specified channel.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(send_broadcast(Channel :: string(), Message :: [supercast:sc_message()]) -> ok).
send_broadcast(Channel, Message) ->
    send_multicast(Channel, Message, default).


%%------------------------------------------------------------------------------
%% @equiv join_ack(Ref, [])
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec(join_accept(Ref :: supercast:sc_reference()) -> ok).
join_accept(Ref) -> join_accept(Ref, []).


%%------------------------------------------------------------------------------
%% @see join_refuse/1
%% @doc
%% Must be called from <em>supercast_proc:join/3</em> to effectively
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
%% @equiv leave_ack(Ref, [])
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec(leave_ack(Ref :: supercast:sc_reference()) -> ok).
leave_ack(Ref) -> leave_ack(Ref, []).


%%------------------------------------------------------------------------------
%% @doc
%% Must be called from <em>supercast_proc:leave_request/3</em> to effectively
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

    ?traceInfo("leave ack"),

    [#chan_state{clients=Clients} = CS] = ets:lookup(?ETS_CHAN_STATES, Channel),
    ?traceInfo("clients 2: ", Clients),
    Clients2 = lists:delete(CState, Clients),
    ?traceInfo("clients 2: ", Clients2),
    ets:insert(?ETS_CHAN_STATES, CS#chan_state{clients=Clients2}).


%%------------------------------------------------------------------------------
%% @see join_accept/2
%% @doc
%% Must be called from <em>supercast_proc:join_request/3</em> to cancel the user
%% request to join the channel. It notify the client with  a
%% <em>subscribeErr</em> message.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(join_refuse(Ref :: supercast:sc_reference()) -> ok).
join_refuse({Channel, #client_state{module=Mod} = CState, QueryId}) ->
    ErrPdu = supercast_endpoint:pdu(subscribeErr, {QueryId, Channel}),
    Mod:send(CState, ErrPdu).



%%%=============================================================================
%%% Utils
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @hidden
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
