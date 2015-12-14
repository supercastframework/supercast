%% -----------------------------------------------------------------------------
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
%% -----------------------------------------------------------------------------

%% @doc Simple authentication module reading "etc/users.xml" file.
-module(supercast_auth_xml).
-behaviour(supercast_auth).
-include_lib("xmerl/include/xmerl.hrl").

-export([authenticate/2]).

-define(USERS_XML, "etc/users.xml").

-spec authenticate(UName :: string(), UPass :: string()) ->
        Reply :: fail | {ok, Groups :: [string()]}.
authenticate(UName, UPass) ->
    {#xmlDocument{content=DocumentContent}, _} = xmerl_scan:file(?USERS_XML, [{document,true}]),
    #xmlElement{content=XmlUsers} = lists:keyfind(xml_users, 2, DocumentContent),

    Users = lists:filter(fun(E) -> is_record(E, xmlElement) end, XmlUsers),
    UDefs = lists:map(fun(#xmlElement{attributes=Attr,content=GContent}) ->
        #xmlAttribute{value=User} = lists:keyfind('Id', 2, Attr),
        #xmlAttribute{value=Pass} = lists:keyfind('Password', 2, Attr),

        Groups = lists:filter(fun(E) -> is_record(E, xmlElement) end, GContent),
        GDefs  = lists:map(fun(#xmlElement{attributes=GAttr}) ->
            #xmlAttribute{value=Group} = lists:keyfind('Id', 2, GAttr),
            Group
        end, Groups),
        
        {User, Pass, GDefs}
    end, Users),

    case lists:keyfind(UName, 1, UDefs) of
        {_, UPass, Groups} ->
            {ok, Groups};
        _ ->
            fail
    end.
