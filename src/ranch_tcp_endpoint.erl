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
%%% @doc
%%% tcp client interface
%%% @end
-module(ranch_tcp_endpoint).
-behaviour(gen_fsm).
-behaviour(ranch_protocol).
-include("supercast.hrl").

% ranch_protocol
-export([start_link/4]).

% supercast
-export([auth_set/2,auth_set/5,send/2,raw_send/2]).

% gen_fsm
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3,
    terminate/3,code_change/4]).

% gen_fsm states
-export(['WAIT_RANCH_ACK'/2,'UNAUTHENTICATED'/2,'AUTHENTICATED'/2]).

-define(TIMEOUT, 30000).
-define(MAX_AUTH_ATEMPT, 3).

-define(ENCODER, jsx).

start_link(Ref, Socket, Transport, Opts) ->
    gen_fsm:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).


%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
auth_set(success,
    #client_state{pid = Pid, ref = Ref}, Name, Roles, AllowedMods) ->
    ?LOG_INFO("auth set"),
    gen_fsm:send_event(Pid, {success, Ref, Name, Roles, AllowedMods}).

auth_set(auth_fail, NewState) ->
    ?LOG_INFO("auth fail", NewState),
    Pid         = NewState#client_state.pid,
    Ref         = NewState#client_state.ref,
    UserName    = NewState#client_state.user_name,
    gen_fsm:send_event(Pid, {auth_fail, Ref, UserName}).

send(SockState, {function, Msg}) ->
    ?LOG_INFO("send", Msg),
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {fexec, SockState#client_state.ref, Msg});

send(SockState, {pdu, Msg}) ->
    ?LOG_INFO("send", Msg),
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {encode_send_msg, SockState#client_state.ref, Msg});

send(SockState, Msg) ->
    ?LOG_INFO("send", Msg),
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {encode_send_msg, SockState#client_state.ref, Msg}).

raw_send(SockState, Pdu) ->
    ?LOG_INFO("raw send", Pdu),
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {send_pdu, SockState#client_state.ref, Pdu}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%------------------------------------------------------------------------
init([RanchRef, Socket, Transport, _Opts]) ->
    process_flag(trap_exit, true),
    State = #client_state{
        ranch_transport = Transport,
        ranch_ref       = RanchRef,
        encoding_mod    = ?ENCODER,
        socket          = Socket,
        ref             = make_ref(),
        pid             = self(),
        module          = ?MODULE,
        authenticated   = false},
    {ok, 'WAIT_RANCH_ACK', State}.

'WAIT_RANCH_ACK'({shoot, RanchRef, Transport, Socket, AckTimeout} = _Ack,
        #client_state{ranch_ref=RanchRef} = State) ->
    ?LOG_INFO("ranch ack", _Ack),
    Transport:accept_ack(Socket, AckTimeout),
    TCPOpts = [{reuseaddr, true}, {keepalive, true}, {packet, 4},
        {send_timeout_close, true}, {active, once}],
    Transport:setopts(Socket, TCPOpts),
    supercast_server:client_msg(connect, State),
    {next_state, 'UNAUTHENTICATED', State, ?TIMEOUT};

'WAIT_RANCH_ACK'({shoot,_RanchRef,_,_,_}, State) ->
    % ignore message for any other ranch refs
    ?LOG_INFO("wait ack unknown ref", _RanchRef),
    {next_state, 'WAIT_RANCH_ACK', State}.

%%-------------------------------------------------------------------------
%% process user credentials here
%%-------------------------------------------------------------------------
'UNAUTHENTICATED'({client_data, Pdu},
        #client_state{encoding_mod=Encoder} = State) ->
    ?LOG_INFO("data received", Pdu),
    supercast_server:client_msg({message, Encoder:decode(Pdu)}, State),
    {next_state, 'UNAUTHENTICATED', State, ?TIMEOUT};

'UNAUTHENTICATED'({success, Ref, Name, Roles, Mods},
        #client_state{ref = Ref} = State) ->
    NextState = State#client_state{
        user_name       = Name,
        user_roles      = Roles,
        user_modules    = Mods,
        authenticated   = true},
    {next_state, 'AUTHENTICATED', NextState};

'UNAUTHENTICATED'({auth_fail, Ref, _User},
        #client_state{ref = Ref} = State) ->
    ?LOG_INFO("Failed to register use", _User),
    {next_state, 'UNAUTHENTICATED', State, ?TIMEOUT};

'UNAUTHENTICATED'(timeout,
        #client_state{auth_request_count = ?MAX_AUTH_ATEMPT} = State) ->
    {stop, normal, State};

'UNAUTHENTICATED'(timeout, State) ->
    NextState = State#client_state{
        auth_request_count = State#client_state.auth_request_count + 1},
    {next_state, 'UNAUTHENTICATED', NextState, ?TIMEOUT};

'UNAUTHENTICATED'(_Data, State) ->
    ?LOG_INFO("Ignoring data", {self(), _Data}),
    {next_state, 'UNAUTHENTICATED', State}.

%%-------------------------------------------------------------------------
%% application running
%%-------------------------------------------------------------------------
'AUTHENTICATED'({client_data, Pdu},
        #client_state{encoding_mod=Encoder} = State) ->
    supercast_server:client_msg({message, Encoder:decode(Pdu)}, State),
    {next_state, 'AUTHENTICATED', State};

'AUTHENTICATED'({synchronize_chan, Ref, Fun},
        #client_state{ref=Ref,ranch_transport=Transport,
            socket=Sock,encoding_mod=Encoder} = State) ->
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
        Pdu = Encoder:encode(Msg),
        Transport:send(Sock, Pdu)
    end, PduList2),
    {next_state, 'AUTHENTICATED', State};

'AUTHENTICATED'(timeout, State) ->
    ?LOG_ERROR("Timeout - closing", self()),
    {stop, normal, State};

'AUTHENTICATED'(_Data, State) ->
    ?LOG_WARNING("Running Ignoring data", {self(), _Data}),
    {next_state, 'AUTHENTICATED', State}.


handle_event({send_pdu, Ref, Pdu}, StateName,
        #client_state{ref=Ref, ranch_transport=Transport, socket=Socket} = State) ->
    Transport:send(Socket, Pdu),
    {next_state, StateName, State};
handle_event({send_pdu, _, _}, StateName, State) ->
    {next_state, StateName, State};

handle_event({encode_send_msg, Ref, Msg}, StateName,
        #client_state{ref=Ref,socket=Socket,
            encoding_mod=Encoder,ranch_transport=Transport} = State) ->
    Pdu = Encoder:encode(Msg),
    Transport:send(Socket, Pdu),
    {next_state, StateName, State};
handle_event({encode_send_msg, _, _}, StateName, State) ->
    {next_state, StateName, State};

handle_event({fexec, Ref, Fun}, StateName,
        #client_state{ref = Ref} = State) ->
    Fun(State),
    {next_state, StateName, State};


handle_event({tcp_error, Reason}, StateName, State) ->
    ?LOG_ERROR("gen_tcp:send/2 error", Reason),
    {stop, {error, Reason, StateName}, State};


handle_event(Event, StateName, StateData) ->
    ?LOG_WARNING("Unknonw event type", {Event, StateName, StateData}),
    {stop, {StateName, undefined_event, Event}, StateData}.



handle_sync_event(Event, _From, StateName, StateData) ->
    ?LOG_WARNING("Unknonw event type", {Event, StateName, StateData}),
    {stop, {StateName, undefined_event, Event}, StateData}.



handle_info({tcp,Socket, Bin}, StateName,
    #client_state{socket=Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({client_data, Bin}, StateData);

handle_info({shoot,_,_,_,_} = Info, 'WAIT_RANCH_ACK', StateData) ->
    gen_fsm:send_event(self(), Info),
    ?MODULE:'WAIT_RANCH_ACK'(Info, StateData);

handle_info({tcp_closed, Socket}, _StateName,
            #client_state{socket=Socket, addr=_Addr} = StateData) ->
    ?LOG_INFO("Client disconnected", [self(), _Addr]),
    {stop, normal, StateData};

handle_info({shoot,_RanchRef,_,_,_}, AnyState, StateData) ->
    % ignore message for any other states
    ?LOG_WARNING("shoot for unknown state", {_RanchRef, AnyState}),
    {next_state, AnyState, StateData};

handle_info(_Info, StateName, StateData) ->
    ?LOG_WARNING("Unknown info", {_Info,StateName,StateData}),
    {stop, StateName, StateData}.



terminate(_Reason, _StateName, State) ->
    ?LOG_INFO("Terminate", {_StateName, _Reason, State}),
    supercast_server:client_msg(disconnect, State),
    ok.



code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
