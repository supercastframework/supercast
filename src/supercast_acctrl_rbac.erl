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

-module(supercast_acctrl_rbac).
-behaviour(supercast_acctrl).

-include("supercast.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([satisfy/3]).

-spec satisfy(Mode :: read | write, [#client_state{}], #perm_conf{}) ->
                                                    {ok, [#client_state{}]}.
%% @doc Role Based Access Control module
%% Givent a list of client and a perm conf, return a list of allowed
%% clients or an empty list.
%% @end
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

%% @private
-spec satisfy_bool([string()], [string()]) -> boolean().
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



