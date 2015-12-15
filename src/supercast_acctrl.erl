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

%%%-----------------------------------------------------------------------------
%%% @author Sebastien Serre <ssbx@supercastframework.org>
%%% @copyright (C) 2015, Sebastien Serre
%%% @doc
%%% Access control for supercast.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(supercast_acctrl).
-include("supercast.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% Filter a list of client satisfying to the permission specified.
%%
%% Return a list of #client_state wich satisfy with the access control
%% constraint defined by term().
%% PermConf is dependant on the return of beha_supercast_auth module
%% used by the application.
%%
%% @end
%%------------------------------------------------------------------------------
-callback(satisfy(Mode :: read | write, Clients :: [#client_state{}],
    PermConf :: term()) -> {ok, Allowed :: [#client_state{}]}).
