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
% @doc
% @end
-module(supercast_users_xml).
-behaviour(supercast_auth).
-include_lib("xmerl/include/xmerl.hrl").

-export([authenticate/2]).

-define(USERS_XML, "etc/users.xml").

%% --------------------------------------------------------------
%% USER API
%% --------------------------------------------------------------
% TODO MAYBE caching scan file?
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
