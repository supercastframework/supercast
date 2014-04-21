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
-module(supercast_auth_ldap).
-behaviour(gen_server).
-behaviour(supercast_auth).
-include_lib("eldap/include/eldap.hrl").
-include("../../include/supercast.hrl").

%% erternal api
-compile(export_all).

-export([authenticate/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% --------------------------------------------------------------
%% external API gen_server
%% --------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

%% --------------------------------------------------------------
%% USER API
%% --------------------------------------------------------------
authenticate(UName, UPass) ->
    case {UName, UPass} of
        {"admuser", "passwd"} ->
            Roles = ["admin","wheel"],
            {ok, Roles};
        {"stduser", "passwd"} ->
            Roles = ["users","wheel"],
            {ok, Roles};
        _ ->
            fail
    end.


%% --------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------
init([]) ->
    % TODO BIND to ldap server
    {ok, []}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast([{fromClient, {authRep, {ldap, Data}}}, SocketState], State) ->
    % TODO ASK ldap server later
    {_, UserName, Password} = Data,
    From    = SocketState#client_state.pid,
    Ref     = SocketState#client_state.ref,
    Mod  = SocketState#client_state.module,
    case {UserName, Password} of
        {"admuser", "passwd"} ->
            % ldap nous donne les groupes auquel apartiens l utilisateur
            Roles = ["group_a","group_b","group_c"],
            % quqchose nous donne les modules auxquels l'utilisateur a access
            Modules = ["test_mod", "monitor"],
            NewSocketState = SocketState#client_state{user_roles=Roles, user_name=UserName},
            Mod:auth_set(auth_success, NewSocketState),
            if_switch:add_client(NewSocketState),
            if_switch:handle_asn(NewSocketState,
                {modIfPDU, {fromServer, {authAck, 
                    {'AuthPDU_fromServer_authAck', Roles, Modules}}}});
        _   ->  
            Mod:auth_set(auth_fail, [From, Ref, UserName]),
            if_switch:handle_asn(SocketState,
                {modIfPDU, {fromServer, {authError, 
                    {'AuthPDU_fromServer_authError', badPass, UserName, Password}}}})
    end,
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
    
code_change(_Old, State, _Ext) ->
    {ok, State}.

% Authenticate the user user_name() on the default localhost server
% local_ldap(_From, User) ->
%     {ok, Handle} = eldap:open([localhost]),
%     ok = eldap:simple_bind(Handle, "cn=Manager,dc=nms,dc=net", "secret"),
%     Filter1 = eldap:equalityMatch("uid", User),
%     Filter2 = eldap:equalityMatch("cn",  User),
%     Filter  = eldap:'and'([Filter1, Filter2]),
%     Rep = eldap:search(Handle, [{base, "dc=nms, dc=net"}, {filter, Filter},
%         {attributes, ["userPassword", "sn"]}, {timeout, 1}]),
%     case Rep of
%         {error, Reason} -> io:format("error ~p ~p~n", [?MODULE, Reason]);
%         {ok, {eldap_search_result, [], []}} -> failure;
%         {ok, SearchResult} ->
%             [Res] = SearchResult#eldap_search_result.entries,
%             % pour le moment on ne recupere que le sn. il faut trouver un moyen de recuperer un ou
%             % des groupes
%             {"sn", FalseRoles} = lists:keyfind("sn", 1, Res#eldap_entry.attributes),
%             {ok, User, FalseRoles}
%     end.



%% --------------------------------------------------------------
%% helpers
%% --------------------------------------------------------------
log(Data) ->
    io:format("log: ~p: ~p~n", [?MODULE, Data]).
