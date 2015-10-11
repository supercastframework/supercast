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
%%% ssl client interface
%%% @end
-module(ssl_client).
-behaviour(gen_fsm).
-include("include/supercast.hrl").
-include_lib("common_hrl/include/logs.hrl").

-export([
    start_link/2,
    set_socket/2,
    auth_set/2,
    auth_set/5,
    send/2,
    raw_send/2
]).

-export([
    init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_CLIENT_AUTH'/2,
    'AUTHENTICATED'/2
]).

-define(TIMEOUT, 30000).
-define(MAX_AUTH_ATEMPT, 3).


start_link(Encoder, SslFiles) ->
    gen_fsm:start_link(?MODULE, [Encoder, SslFiles], []).


%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

auth_set(success,
    #client_state{pid = Pid, ref = Ref}, Name, Roles, AllowedMods) ->
    gen_fsm:send_event(Pid, {success, Ref, Name, Roles, AllowedMods}).

auth_set(auth_fail,     NewState) ->
    Pid         = NewState#client_state.pid,
    Ref         = NewState#client_state.ref,
    UserName    = NewState#client_state.user_name,
    gen_fsm:send_event(Pid, {auth_fail, Ref, UserName}).

send(SockState, {function, Msg}) ->
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {fexec, SockState#client_state.ref, Msg});

send(SockState, {pdu, Msg}) ->
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {encode_send_msg, SockState#client_state.ref, Msg});

send(SockState, Msg) ->
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {encode_send_msg, SockState#client_state.ref, Msg}).

raw_send(SockState, Pdu) ->
    gen_fsm:send_all_state_event(SockState#client_state.pid,
        {send_pdu, SockState#client_state.ref, Pdu}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%------------------------------------------------------------------------
init([Encoder, {Key, Cert, CACert}]) ->
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #client_state{
        encoding_mod        = Encoder,
        communication_mod   = ssl,
        key                 = Key,
        ca_certificate      = CACert,
        certificate         = Cert,
        state               = 'WAIT_FOR_SOCKET'}}.


'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    %% Now we own the socket!
    {ok, SSLSocket} = ssl:ssl_accept(Socket, [
        {cacertfile,        State#client_state.ca_certificate},
        {certfile,          State#client_state.certificate},
        {keyfile,           State#client_state.key},
        {protocol,          tlsv1},
        {active,            false}], 3000),

    ssl:setopts(SSLSocket, [{active, once}]),
    {ok, {IP, Port}} = inet:peername(Socket),
    NextState = State#client_state{
        socket          = SSLSocket,
        addr            = IP,
        port            = Port,
        ref             = make_ref(),
        pid             = self(),
        module          = ?MODULE,
        state           = 'WAIT_FOR_CLIENT_AUTH'},
    supercast_server:client_msg(connect, NextState),
    {next_state, 'WAIT_FOR_CLIENT_AUTH', NextState, ?TIMEOUT};

'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State:'WAIT_FOR_SOCKET'. Unexpected message:~p\n",
            [Other]),
    {next_state, 'WAIT_FOR_SOCKET', State}.


%%-------------------------------------------------------------------------
%% process user credentials here
%%-------------------------------------------------------------------------
'WAIT_FOR_CLIENT_AUTH'({client_data, Pdu},
        #client_state{encoding_mod = Encoder} = State) ->
    supercast_server:client_msg({message, Encoder:decode(Pdu)}, State),
    {next_state, 'WAIT_FOR_CLIENT_AUTH', State, ?TIMEOUT};

'WAIT_FOR_CLIENT_AUTH'({success, Ref, Name, Roles, Mods},
        #client_state{ref = Ref} = State) ->
    NextState = State#client_state{
        user_name       = Name,
        user_roles      = Roles,
        user_modules    = Mods,
        state           = 'AUTHENTICATED'},
    {next_state, 'AUTHENTICATED', NextState};

'WAIT_FOR_CLIENT_AUTH'({auth_fail, Ref, _User},
        #client_state{ref = Ref} = State) ->
    ?LOG_INFO("failed to register with user:", _User),
    {next_state, 'WAIT_FOR_CLIENT_AUTH', State, ?TIMEOUT};

'WAIT_FOR_CLIENT_AUTH'(timeout,
        #client_state{auth_request_count = ?MAX_AUTH_ATEMPT} = State) ->
    {stop, normal, State};

'WAIT_FOR_CLIENT_AUTH'(timeout, State) ->
    NextState = State#client_state{
        auth_request_count = State#client_state.auth_request_count + 1},
    supercast_server:client_msg(connect, NextState),
    {next_state, 'WAIT_FOR_CLIENT_AUTH', NextState, ?TIMEOUT};

'WAIT_FOR_CLIENT_AUTH'(_Data, State) ->
    ?LOG_WARNING("Ignoring data: ", {self(), _Data}),
    {next_state, 'WAIT_FOR_CLIENT_AUTH', State}.

%%-------------------------------------------------------------------------
%% application running
%%-------------------------------------------------------------------------
'AUTHENTICATED'({client_data, Pdu},
        #client_state{encoding_mod = Encoder} = State) ->
    supercast_server:client_msg({message, Encoder:decode(Pdu)}, State),
    {next_state, 'AUTHENTICATED', State};

'AUTHENTICATED'({synchronize_chan, Ref, Fun}, #client_state{
        ref             = Ref,
        socket          = Sock,
        encoding_mod    = Encoder} = State
    ) ->
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
        ssl:send(Sock, Pdu)
    end, PduList2),
    {next_state, 'AUTHENTICATED', State};

'AUTHENTICATED'(timeout, State) ->
    error_logger:error_msg("~p Timeout - closing.\n", [self()]),
    {stop, normal, State};

'AUTHENTICATED'(_Data, State) ->
    ?LOG_INFO("Running Ignoring data:", {self(), _Data}),
    {next_state, 'AUTHENTICATED', State}.


handle_event({send_pdu, Ref, Pdu}, StateName,
        #client_state{ref = Ref} = State) ->
    Socket  = State#client_state.socket,
    ssl:send(Socket, Pdu),
    {next_state, StateName, State};
handle_event({send_pdu, _, _}, StateName, State) ->
    {next_state, StateName, State};

handle_event({encode_send_msg, Ref, Msg}, StateName,
        #client_state{ref = Ref} = State) ->
    Socket  = State#client_state.socket,
    Encoder = State#client_state.encoding_mod,
    Pdu = Encoder:encode(Msg),
    ssl:send(Socket, Pdu),
    {next_state, StateName, State};
handle_event({encode_send_msg, _, _}, StateName, State) ->
    {next_state, StateName, State};

handle_event({fexec, Ref, Fun}, StateName,
        #client_state{ref = Ref} = State) ->
    Fun(State),
    {next_state, StateName, State};


handle_event({ssl_error, Reason}, StateName, State) ->
    error_logger:info_msg("~p ~p ssl:send/2 error: ~p",
        [?MODULE,?LINE,Reason]),
    {stop, {error, Reason, StateName}, State};


handle_event(Event, StateName, StateData) ->
    ?LOG_ERROR("Handle unknown event", {Event, StateData}),
    {stop, {StateName, undefined_event, Event}, StateData}.



handle_sync_event(Event, _From, StateName, StateData) ->
    ?LOG_ERROR("Handle unknown event", {Event, _From, StateData}),
    {stop, {StateName, undefined_event, Event}, StateData}.



handle_info({ssl,Socket, Bin}, StateName,
    #client_state{socket=Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({client_data, Bin}, StateData);

handle_info({ssl_closed, Socket}, _StateName,
            #client_state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    ?LOG_WARNING("Unknown info", {StateName,_Info,StateData}),
    {noreply, StateName, StateData}.



terminate(_Reason, _StateName, #client_state{socket=Socket} = State) ->
    ?LOG_INFO("Terminate:", {self(), _Reason, _StateName, State}),
    (catch ssl:close(Socket)),
    supercast_server:client_msg(disconnect, State),
    ok.



code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
