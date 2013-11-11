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
-module(tcp_client).
-behaviour(gen_fsm).
-include("../include/supercast.hrl").

-export([
    start_link/1,
    set_socket/2,
    auth_set/2,
    auth_set/5,
    send/2,
    raw_send/2,
    synchronize/2
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
    'RUNNING'/2
]).

-define(TIMEOUT, 30000).
-define(MAX_AUTH_ATEMPT, 3).
-define(ENCODING_MOD, bsupercast_encoder_asn).


%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec start_link(Encoder) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%	  If init/1 fails with Reason, the function returns {error,Reason}.
%%	  If init/1 returns {stop,Reason} or ignore, the process is
%%	  terminated and the function returns {error,Reason} or ignore,
%%	  respectively.
%% @end
%%-------------------------------------------------------------------------
start_link([Encoder]) ->
	gen_fsm:start_link(?MODULE, [Encoder], []).

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

send(SockState, Msg) ->
    gen_fsm:send_event(SockState#client_state.pid,
        {encode_send_msg, SockState#client_state.ref, Msg}).

raw_send(SockState, Pdu) ->
    gen_fsm:send_event(SockState#client_state.pid, 
        {send_pdu, SockState#client_state.ref, Pdu}).

synchronize(SockState, Fun) ->
    gen_fsm:send_event(SockState#client_state.pid, 
        {synchronize_chan, SockState#client_state.ref, Fun}).
%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}		  |
%%		  {ok, StateName, StateData, Timeout} |
%%		  ignore							  |
%%		  {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([Encoder]) ->
	process_flag(trap_exit, true),
	{ok, 'WAIT_FOR_SOCKET', #client_state{
        encoding_mod    = Encoder,
        state           = 'WAIT_FOR_SOCKET'}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NextStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
	%% Now we own the socket!
	inet:setopts(Socket, [{active, once}, {packet, 4}, binary]),
	{ok, {IP, Port}} = inet:peername(Socket),
    NextState = State#client_state{
        socket          = Socket,
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
        state           = 'RUNNING'},
    {next_state, 'RUNNING', NextState};

'WAIT_FOR_CLIENT_AUTH'({auth_fail, Ref, User}, 
        #client_state{ref = Ref} = State) ->
	io:format("failed to register with user ~p ~n", [User]),
    {next_state, 'WAIT_FOR_CLIENT_AUTH', State, ?TIMEOUT};

'WAIT_FOR_CLIENT_AUTH'({encode_send_msg, Ref,  Msg},  
        #client_state{ref = Ref, encoding_mod = Encoder} = State) ->
    gen_tcp:send(State#client_state.socket, Encoder:encode(Msg)),
    {next_state, 'WAIT_FOR_CLIENT_AUTH', State};

'WAIT_FOR_CLIENT_AUTH'({send_pdu, Ref,  Pdu},  
        #client_state{ref = Ref} = State) ->
    gen_tcp:send(State#client_state.socket, Pdu),
    {next_state, 'WAIT_FOR_CLIENT_AUTH', State};

'WAIT_FOR_CLIENT_AUTH'(timeout, 
        #client_state{auth_request_count = ?MAX_AUTH_ATEMPT} = State) ->
	{stop, normal, State};

'WAIT_FOR_CLIENT_AUTH'(timeout, State) ->
    NextState = State#client_state{
        auth_request_count = State#client_state.auth_request_count + 1},
    supercast_server:client_msg(connect, NextState),
	{next_state, 'WAIT_FOR_CLIENT_AUTH', NextState, ?TIMEOUT};

'WAIT_FOR_CLIENT_AUTH'(Data, State) ->
	io:format("~p Ignoring data: ~p\n", [self(), Data]),
	{next_state, 'WAIT_FOR_CLIENT_AUTH', State}.

%%-------------------------------------------------------------------------
%% application running
%%-------------------------------------------------------------------------
'RUNNING'({client_data, Pdu}, 
        #client_state{encoding_mod = Encoder} = State) ->
    supercast_server:client_msg({message, Encoder:decode(Pdu)}, State),
	{next_state, 'RUNNING', State};

'RUNNING'({encode_send_msg, Ref, Msg}, 
        #client_state{ref = Ref, encoding_mod = Encoder} = State) ->
    gen_tcp:send(State#client_state.socket, Encoder:encode(Msg)),
    {next_state, 'RUNNING', State};

'RUNNING'({send_pdu, Ref, Pdu}, #client_state{ref = Ref} = State) ->
    gen_tcp:send(State#client_state.socket, Pdu),
    {next_state, 'RUNNING', State};

'RUNNING'({synchronize_chan, Ref, Fun}, #client_state{
        ref             = Ref,
        socket          = Sock,
        encoding_mod    = Encoder} = State
    ) ->
    {ok, PduList} = Fun(),
    lists:foreach(fun(Pdu) -> 
        gen_tcp:send(Sock, Encoder:encode(Pdu))
    end, PduList),
    {next_state, 'RUNNING', State};

'RUNNING'(timeout, State) ->
	error_logger:error_msg("~p Timeout - closing.\n", [self()]),
	{stop, normal, State};

'RUNNING'(Data, State) ->
	io:format("~p Running Ignoring data: ~p\n", [self(), Data]),
	{next_state, 'RUNNING', State}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NextStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
	io:format("handle_event ~p ~p~n", [?MODULE, StateData]),
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}			|
%%		  {next_state, NextStateName, NextStateData, Timeout}   |
%%		  {reply, Reply, NextStateName, NextStateData}		  |
%%		  {reply, Reply, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NextStateData}						  |
%%		  {stop, Reason, Reply, NextStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
	io:format("handle_sync_event ~p ~p~n", [?MODULE, StateData]),
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NextStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp,Socket, Bin}, StateName, 
    #client_state{socket=Socket} = StateData) ->
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({client_data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
			#client_state{socket=Socket, addr=Addr} = StateData) ->
	error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info(Info, StateName, StateData) ->
    io:format("INFO: ~p~n", [Info]),
    io:format("STATENAME: ~p~n", [StateName]),
    io:format("STATEDATA: ~p~n", [StateData]),
	io:format("handle_info 3 ~p ~p ~p~n", [?MODULE, StateData, Info]),
	{noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #client_state{socket=Socket} = State) ->
	io:format("terminate ~p ~p~n", [?MODULE, _Reason]),
	(catch gen_tcp:close(Socket)),
    supercast_server:client_msg(disconnect, State),
	ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NextState, NextStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
	io:format("code_change ~p ~p~n", [?MODULE, StateData]),
	{ok, StateName, StateData}.
