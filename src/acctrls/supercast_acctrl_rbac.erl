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
-module(supercast_acctrl_rbac).
-behaviour(supercast_acctrl).

-include("../../include/supercast.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-export([satisfy/3]).

-spec satisfy(read | write, [#client_state{}], #perm_conf{}) -> 
        {ok, [#client_state{}]}.
% @private
% @doc
% Givent a list of client and a perm conf, return a list of allowed
% clients or an empty list.
% @end
satisfy(read, ClientList, #perm_conf{read = ReadList}) ->
    {ok, satisfy_pass(ReadList, ClientList, [])};
satisfy(write, ClientList, #perm_conf{write = WriteList}) ->
    {ok, satisfy_pass(WriteList, ClientList, [])}.


-spec satisfy_pass([atom()], [#client_state{}], [#client_state{}]) ->
        [#client_state{}].
satisfy_pass(_ReadList, [], AllowedClientList) ->
    AllowedClientList;

satisfy_pass(ReadList, 
        [#client_state{user_roles = Roles} = Client | ClientList], 
        AllowedClientList) ->
    case satisfy_bool(Roles, ReadList) of
        true    -> 
            satisfy_pass(ReadList, ClientList, [Client | AllowedClientList]);
        false   ->
            satisfy_pass(ReadList, ClientList, AllowedClientList)
    end.

-spec satisfy_bool([term()], [term()]) -> true | false.
satisfy_bool([], _RoleListTwo) ->
    false;
satisfy_bool([R | RoleListOne], RoleListTwo) ->
    case lists:member(R, RoleListTwo) of
        true    -> true;
        false   -> satisfy_bool(RoleListOne, RoleListTwo)
    end.

%% TESTS
satisfy_test() ->
    Ca = #client_state{user_roles = ["roleA", "roleC", "roleX"]},
    Cb = #client_state{user_roles = ["roleB", "roleC"]},
    ?assertMatch({ok, [Ca]},
        satisfy(read, [Ca],    #perm_conf{read = ["roleA", "roleD"]})),
    ?assertMatch({ok, [Ca]},
        satisfy(read, [Ca,Cb], #perm_conf{read = ["roleA", "roleD"]})),
    ?assertMatch({ok, [Cb]},    
        satisfy(write,[Ca,Cb], #perm_conf{write = ["roleB", "roleD"]})),
    ?assertMatch({ok, [Cb,Ca]}, 
        satisfy(read, [Ca,Cb], #perm_conf{read = ["roleC", "roleB"]})),
    ?assertMatch({ok, [Cb,Ca]}, 
        satisfy(write,[Ca,Cb], #perm_conf{write = ["roleB", "roleX"]})),
    ?assertMatch({ok, []},      
        satisfy(read, [Ca,Cb], #perm_conf{read = ["roleD"]})).



