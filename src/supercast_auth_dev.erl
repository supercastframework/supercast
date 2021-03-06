%% -----------------------------------------------------------------------------
%% Supercast Copyright (c) 2012-2016
%% Sebastien Serre <ssbx@sysmo.io> All Rights Reserved.
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

%%%-----------------------------------------------------------------------------
%%% @author Sebastien Serre <ssbx@sysmo.io>
%%% @copyright (C) 2015, Sebastien Serre
%%% @doc
%%% Development authentication module always returning
%%% {ok, ["admin","wheel","other"]}
%%%
%%% @end
%%% ----------------------------------------------------------------------------
-module(supercast_auth_dev).
-behaviour(supercast_auth).
-export([authenticate/2]).

-spec authenticate(UName::string(), UPass::string()) ->
        {ok, Groups::[string()]}.
authenticate(_AnyName, _AnyPass) ->
    Groups = ["admin", "wheel", "other"],
    {ok, Groups}.
