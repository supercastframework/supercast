%% -------------------------------------------------------------------
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
%% -------------------------------------------------------------------

%% @doc
%% A module implementing this behaviour must export the function statisfy/3.
%% statisfy/2 must take as arguments read | write a list of #client_state
%% and a term().
%% Return a list of #client_state wich satisfy with the access control
%% constraint defined by term().
%% term() is dependant on the return of beha_supercast_auth module wich is used by
%% the application.
%% @end
-module(supercast_acctrl).

-callback satisfy(Mod :: read | write, Things::[any()], PermConf::any()) ->
    {ok, Allowed::[any()]} |
    {ok, Allowed::[]}.
