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

%% @doc
%% A module using this behaviour can be used by <em>supercast</em> to
%% authenticate clients. The return will be used by the beha_supercast_acctrl
%% module used by the application
%% 
%% The only function to be exported is <em>authenticate/2</em>.
%% <code>
%% authenticate(Uname, UPass) -> Any::term() | fail
%% </code>
%%
%% Note that the returned term() can be anything but must be understandable
%% by the beha_supercast_acctrl module used by the application.
%% @end
-module(supercast_auth).

-callback authenticate(UName::string(), UPass::string()) ->
    {ok, Roles::[string()]} | fail.
