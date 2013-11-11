% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Based on the work from Serge Aleynikov <saleyn at gmail.com> on the article
% from http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
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
%% @doc 'etcp_listener' listen to a port and give the connexion from the
%% outside to another module (defined in start_link/3).
-module(ssl_listener).
-behaviour(gen_server).

%% External API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
                listener, % Listening socket
                acceptor, % Asynchronous acceptor's internal reference
                module, % FSM handling module
                connection_limit % max connection
               }).

%%--------------------------------------------------------------------
%% @spec (Port::integer(), Module, Connlimit) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Port, Module, ConnLimit) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, 
            [Port, Module, ConnLimit], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------
init([Port, Module, ConnLimit]) ->
    process_flag(trap_exit, true),
    Opts = [binary, {packet, 2}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
    {ok, Listen_socket} ->
        %%Create first accepting process
        {ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
        {ok, #state{listener = Listen_socket,
                    acceptor = Ref,
                    module   = Module,
                    connection_limit = ConnLimit}};
    {error, Reason} ->
        {stop, Reason}
    end.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}}, #state{
        listener            = ListSock, 
        acceptor            = Ref, 
        module              = Module, 
        connection_limit    = ConnLimit} = State) ->

    ActiveC = erlang:length(supervisor:which_children(ssl_client_sup)),
    %% test the max conn limit
    case ActiveC < ConnLimit of
        true ->
            try
                case set_sockopt(ListSock, CliSocket) of
                ok               -> ok;
                {error, Reason} -> exit({set_sockopt, Reason})
                end,

                %% New client connected - spawn a new process using the 
                %% simple_one_for_one supervisor.
                {ok, Pid} = ssl_client_sup:start_client(),
                gen_tcp:controlling_process(CliSocket, Pid),
                %% Instruct the new FSM that it owns the socket.
                Module:set_socket(Pid, CliSocket),
        
                %% Signal the network driver that we are ready to accept 
                %% another connection
                case prim_inet:async_accept(ListSock, -1) of
                    {ok,    NewRef} -> 
                        ok;
                    {error, NewRef} -> 
                        exit({async_accept, inet:format_error(NewRef)})
                end,

                {noreply, State#state{acceptor=NewRef}}
            catch exit:Why ->
                error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
                {stop, Why, State}
            end;
        false ->
            gen_tcp:close(CliSocket),
            error_logger:warning_msg("~p connexion limit exeded~n", 
                    [CliSocket]),
            case prim_inet:async_accept(ListSock, -1) of
                {ok,    NewRef} ->
                    ok;
                {error, NewRef} -> 
                    exit({async_accept, inet:format_error(NewRef)})
            end,
            {noreply, State#state{acceptor=NewRef}}
    end;

handle_info({inet_async, ListSock, Ref, Error}, 
        #state{listener=ListSock, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, 
            [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.
