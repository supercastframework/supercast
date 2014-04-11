% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
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
-module(supercast_server).
-behaviour(gen_server).
-include("../include/supercast.hrl").

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
    start_link/1,
    client_msg/2
]).

-record(state, {
    auth_mod,
    dispatch
}).
 

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
start_link(ServerConf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ServerConf, []).


which_auth() ->
    gen_server:call(?MODULE, {get, auth_mod}).


% API FROM CLIENTS
client_msg(Msg, ClientState) ->
    handle_client_msg(Msg, ClientState).

% from himself
handle_client_command(Mod, Msg, CState) ->
    gen_server:call(?MODULE, {client_command, Mod, Msg, CState}).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
init({AuthModule, PduDispatch}) ->
    ?LOGS({"pdu_dispatch", PduDispatch}),
    {ok, #state{
        auth_mod = AuthModule,
        dispatch = PduDispatch
        }
    }.

% @private
handle_call(dump, _F, S) ->
    {reply, {ok, S}, S};

handle_call({client_command, Key, Msg, CState}, _F, 
        #state{dispatch = Dispatch} = S) ->
    case lists:keyfind(Key, 2, Dispatch) of
        false ->
            {reply, ok, S};
        {Module, Key} ->
            Module:handle_command(Msg, CState),
            {reply, ok, S}
    end;

handle_call({get, auth_mod}, _F, #state{auth_mod = AuthMod} = S) ->
    {reply, AuthMod, S};

% CLIENT RELATED CALLS
handle_call(_R, _F, S) ->
    io:format("handle_call ~p~p~n", [?MODULE, _R]),
    {noreply, S}.

% CAST
% @private
handle_cast(_R, S) ->
    io:format("handle_cast ~p~p~n", [?MODULE, _R]),
    {noreply, S}.

% OTHER
% @private
handle_info(_I, S) ->
    {noreply, S}.

% @private
terminate(_R, _S) ->
    normal.

% @private
code_change(_O, S, _E) ->
    {ok, S}.

% CLIENT CALL
% @private
handle_client_msg(connect, ClientState) ->
    send(ClientState, pdu(authReq, ldap));

handle_client_msg(disconnect, ClientState) ->
    supercast_mpd:client_disconnect(ClientState);

handle_client_msg(
        {message, 
            {modSupercastPDU, 
                {fromClient,
                    {authResp,
                        {'AuthResp', 
                            Name,
                            Password
        }   }   }   }   },  #client_state{module = CMod} = ClientState) ->
    AuthMod = which_auth(),
    case AuthMod:authenticate(Name, Password) of
        {ok, Groups} ->
            MainChans = supercast_mpd:main_chans(),
            CMod:auth_set(success, ClientState, Name, Groups, MainChans),
            send(ClientState,pdu(authAck, {Groups, MainChans}));
        fail    ->
            send(ClientState, pdu(authErr, {Name, Password}))
    end;

handle_client_msg(
        {message, 
            {modSupercastPDU, 
                {fromClient,
                    {subscribe,
                        {'Subscribe',
                            QueryId,
                            Channel
        }   }   }   }   }, CState) ->
    Chan = erlang:list_to_atom(Channel),
    % ok mean satisfy return ok for read the chan
    case supercast_mpd:subscribe_stage1(Chan, CState) of
        error ->
            send(CState, pdu(subscribeErr, {QueryId, Channel}));
        ok ->
            send(CState, pdu(subscribeOk, {QueryId, Channel})),
            supercast_mpd:subscribe_stage2(Chan,CState)
    end;
            
handle_client_msg(
        {message, 
            {modSupercastPDU, 
                {fromClient,
                    {unsubscribe,
                        {'Unsubscribe',
                            QueryId,
                            Channel
        }   }   }   }   }, CState) ->
    Chan = erlang:list_to_atom(Channel),
    case supercast_mpd:unsubscribe(Chan, CState) of
        ok  ->
            send(CState, pdu(unsubscribeOk, {QueryId, Channel}));
        _   ->
            send(CState, pdu(unsubscribeErr, {QueryId, Channel}))
    end;

handle_client_msg(
        {message, 
            {modSupercastPDU, 
                Other
        }   }, _) ->
    io:format("~p RECEIVED UNKNOWN supercast ~p~n", [?MODULE, Other]);

handle_client_msg(
                {message,
                    {Mod,
                        _
                } = Msg  }, CState) ->
   handle_client_command(Mod, Msg, CState). 

% server PDUs
% @private
pdu(authReq, Type) ->
    {modSupercastPDU, 
        {fromServer, 
            {authReq, 
                Type }}};

pdu(authAck, {Groups, StaticChans}) ->
    StaticChansAsString = lists:foldl(fun(Atom, Accum) ->
        [{'ChanInfo', atom_to_list(Atom), create} | Accum]
    end, [], StaticChans),
    {modSupercastPDU, 
        {fromServer,
            {authAck,
                {'AuthAck',
                    Groups,
                    StaticChansAsString }}}};

pdu(authErr, {Name, Password}) ->
    {modSupercastPDU,
        {fromServer,
            {authError,
                {'AuthError',
                    badPass,
                    Name,
                    Password }}}};

pdu(subscribeOk, {QueryId, Channel}) ->
    {modSupercastPDU, 
        {fromServer, 
            {subscribeOk, 
                {'SubscribeOk',
                    QueryId,
                    Channel  }}}};

pdu(subscribeErr, {QueryId, Channel}) ->
    {modSupercastPDU, 
        {fromServer,
            {subscribeErr, 
                {'SubscribeErr',
                    QueryId,
                    Channel }}}};

pdu(unsubscribeOk, {QueryId, Channel}) ->
    {modSupercastPDU, 
        {fromServer, 
            {unsubscribeOk, 
                {'UnsubscribeOk',
                    QueryId,
                    Channel }}}};

pdu(unsubscribeErr, {QueryId, Channel}) ->
    {modSupercastPDU, 
        {fromServer, 
            {unsubscribeErr, 
                {'UnsubscribeErr',
                    QueryId,
                    Channel }}}};

pdu(chanInfo, {Module, Channel, Type}) ->
    {modSupercastPDU,
        {fromServer,
            {chanInfo,
                {'ChanInfo',
                    Module,
                    Channel,
                    Type }}}}.


send(#client_state{module = CMod} = ClientState, Msg) ->
    CMod:send(ClientState, Msg).
