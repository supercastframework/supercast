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

-define(ENCODER, jsx).
-define(ETS_CHAN_STATES, supercast_channels_store).
-define(ETS_DYNAMIC_CHAN_STATES, supercast_dynamic_channels_store).

-record(perm_conf, {
    read  = []  :: [string()],
    write = []  :: [string()]
}).


-record(client_state,  {
    pid                     :: pid(),
    ref                     :: reference(),
    user_name = ""          :: string(),
    user_roles = []         :: [string()],
    module                  :: atom(),
    authenticated = false   :: boolean(),
    data                    :: any() %% used to store cowboy state
}).

-record(chan_state, {
    name         :: string(),
    perm         :: #perm_conf{},
    module       :: atom(),
    args    = [] :: any(),
    clients = [] :: [#client_state{}]
}).


-ifdef(eqc).
-include("eqc/include/eqc.hrl").
-include("eqc/include/eqc_fsm.hrl").
-endif.
%% logger
-ifdef(debug).
-define(traceInfo(String,Term),
    error_logger:info_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).

-define(traceInfo(String),
    error_logger:info_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).

-define(traceWarning(String,Term),
    error_logger:warning_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).

-define(traceWarning(String),
    error_logger:warning_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).
-else.
-define(traceInfo(String), ok).
-define(traceInfo(String,Term), ok).
-define(traceWarning(String), ok).
-define(traceWarning(String,Term), ok).
-endif.

-define(SUPERCAST_LOG_ERROR(String,Term),
    error_logger:error_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).

-define(SUPERCAST_LOG_ERROR(String),
    error_logger:error_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).
