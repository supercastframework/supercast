%% -------------------------------------------------------------------
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
%% -------------------------------------------------------------------

%% @doc Tcp client interface
-module(supercast_endpoint_tcp).
-behaviour(gen_fsm).
-behaviour(ranch_protocol).
-include("supercast.hrl").

%% ranch_protocol
-export([start_link/4]).

%% supercast
-export([auth_success/3,send/2,raw_send/2]).

%% gen_fsm
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3,
    terminate/3,code_change/4]).

%% gen_fsm states
-export(['WAIT_RANCH_ACK'/2,'UNAUTHENTICATED'/2,'AUTHENTICATED'/2]).

-define(TIMEOUT, 30000).
-define(MAX_AUTH_ATEMPT, 3).

-define(ENCODER, jsx).

start_link(Ref, Socket, Transport, Opts) ->
    gen_fsm:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).


%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
auth_success(#client_state{pid = Pid, ref = Ref}, Name, Roles) ->
    ?SUPERCAST_LOG_INFO("auth set"),
    gen_fsm:send_event(Pid, {auth_success, Ref, Name, Roles}).

send(SockState, {function, Msg}) ->
    ?SUPERCAST_LOG_INFO("send", Msg),
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {fexec, SockState#client_state.ref, Msg});

send(SockState, {pdu, Msg}) ->
    ?SUPERCAST_LOG_INFO("send", Msg),
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {encode_send_msg, SockState#client_state.ref, Msg});

send(SockState, Msg) ->
    ?SUPERCAST_LOG_INFO("send", Msg),
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {encode_send_msg, SockState#client_state.ref, Msg}).

raw_send(SockState, Pdu) ->
    ?SUPERCAST_LOG_INFO("raw send", Pdu),
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {send_pdu, SockState#client_state.ref, Pdu}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%------------------------------------------------------------------------
init([RanchRef, Socket, Transport, _Opts]) ->
    process_flag(trap_exit, true),
    State = #client_state{
        ref             = make_ref(),
        pid             = self(),
        module          = ?MODULE,
        authenticated   = false,
        data            = {Socket, Transport, RanchRef}
    },
    {ok, 'WAIT_RANCH_ACK', State}.

'WAIT_RANCH_ACK'({shoot, RanchRef, Transport, Socket, AckTimeout} = _Ack,
        #client_state{data={_,_,RanchRef}} = State) ->
    ?SUPERCAST_LOG_INFO("ranch ack", _Ack),
    Transport:accept_ack(Socket, AckTimeout),
    TCPOpts = [{reuseaddr, true}, {keepalive, true}, {packet, 4},
        {send_timeout_close, true}, {active, once}],
    Transport:setopts(Socket, TCPOpts),
    PduTerm = supercast_endpoint:init_pdu(),
    Transport:send(?ENCODER:encode(PduTerm), Socket),
    {next_state, 'UNAUTHENTICATED', State, ?TIMEOUT};

'WAIT_RANCH_ACK'({shoot,_RanchRef,_,_,_}, State) ->
    % ignore message for any other ranch refs
    ?SUPERCAST_LOG_INFO("wait ack unknown ref", _RanchRef),
    {next_state, 'WAIT_RANCH_ACK', State}.

%%-------------------------------------------------------------------------
%% process user credentials here
%%-------------------------------------------------------------------------
'UNAUTHENTICATED'({client_data, Pdu}, State) ->
    ?SUPERCAST_LOG_INFO("data received", Pdu),
    supercast_endpoint:handle_message(?ENCODER:decode(Pdu), State),
    {next_state, 'UNAUTHENTICATED', State, ?TIMEOUT};

'UNAUTHENTICATED'({auth_success, Ref, Name, Roles},
        #client_state{ref = Ref} = State) ->
    NextState = State#client_state{
        user_name       = Name,
        user_roles      = Roles,
        authenticated   = true},
    {next_state, 'AUTHENTICATED', NextState};

'UNAUTHENTICATED'(_Data, State) ->
    ?SUPERCAST_LOG_INFO("Ignoring data", {self(), _Data}),
    {next_state, 'UNAUTHENTICATED', State}.

%%-------------------------------------------------------------------------
%% application running
%%-------------------------------------------------------------------------
'AUTHENTICATED'({client_data, Pdu}, State) ->
    supercast_endpoint:handle_message(?ENCODER:decode(Pdu), State),
    {next_state, 'AUTHENTICATED', State};

'AUTHENTICATED'({synchronize_chan, Ref, Fun},
        #client_state{ref=Ref, data={Socket,Transport,_}} = State) ->
    {ok, PduList} = Fun(),
    PduList2 = lists:filter(fun(X) ->
        case X of
            ignore ->
                false;
            _ ->
                true
        end
    end, PduList),
    lists:foreach(fun(Msg) ->
        Pdu = ?ENCODER:encode(Msg),
        Transport:send(Socket, Pdu)
    end, PduList2),
    {next_state, 'AUTHENTICATED', State};

'AUTHENTICATED'(timeout, State) ->
    ?SUPERCAST_LOG_ERROR("Timeout - closing", self()),
    {stop, normal, State};

'AUTHENTICATED'(_Data, State) ->
    ?SUPERCAST_LOG_WARNING("Running Ignoring data", {self(), _Data}),
    {next_state, 'AUTHENTICATED', State}.


handle_event({send_pdu, Ref, Pdu}, StateName,
        #client_state{ref=Ref, data={Socket,Transport,_}} = State) ->
    Transport:send(Socket, Pdu),
    {next_state, StateName, State};

handle_event({encode_send_msg, Ref, Msg}, StateName,
        #client_state{ref=Ref, data={Socket,Transport,_}} = State) ->
    Pdu = ?ENCODER:encode(Msg),
    Transport:send(Socket, Pdu),
    {next_state, StateName, State};

handle_event({fexec, Ref, Fun}, StateName, #client_state{ref=Ref} = State) ->
    Fun(State),
    {next_state, StateName, State};

handle_event({tcp_error, Reason}, StateName, State) ->
    ?SUPERCAST_LOG_ERROR("gen_tcp:send/2 error", Reason),
    {stop, {error, Reason, StateName}, State};


handle_event(Event, StateName, StateData) ->
    ?SUPERCAST_LOG_WARNING("Unknonw event type", {Event, StateName, StateData}),
    {stop, {StateName, undefined_event, Event}, StateData}.



handle_sync_event(Event, _From, StateName, StateData) ->
    ?SUPERCAST_LOG_WARNING("Unknonw event type", {Event, StateName, StateData}),
    {stop, {StateName, undefined_event, Event}, StateData}.



handle_info({tcp,Socket, Bin}, StateName,
    #client_state{data={Socket,_,_}} = State) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({client_data, Bin}, State);

handle_info({shoot,_,_,_,_} = Info, 'WAIT_RANCH_ACK', StateData) ->
    gen_fsm:send_event(self(), Info),
    ?MODULE:'WAIT_RANCH_ACK'(Info, StateData);

handle_info({tcp_closed, _Socket}, _StateName, StateData) ->
    ?SUPERCAST_LOG_INFO("Client disconnected", self()),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    ?SUPERCAST_LOG_WARNING("Unknown info", {_Info,StateName,StateData}),
    {stop, StateName, StateData}.



terminate(_Reason, _StateName, State) ->
    ?SUPERCAST_LOG_INFO("Terminate", {_StateName, _Reason, State}),
    supercast_endpoint:client_disconnected(State),
    ok.



code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
