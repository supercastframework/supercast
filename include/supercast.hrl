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

-type json_jsx() :: [any()].
-type supercast_msg() :: json_jsx().

-define(ENCODER, jsx).
-define(ETS_CHAN_STATES, chan_states).
-define(ETS_RELAYS_REGISTER, relays_register).

-record(perm_conf, {
    read    = []    :: [term()],
    write   = []    :: [term()]
}).

-record(chan_state, {
    name,
    perm,
    module,
    args
}).

-record(client_state,  {
    pid,
    ref,
    user_name = "",
    user_roles = [],
    module,
    authenticated = false,
    data
}).





%% logger
-ifdef(debug).
-define(SUPERCAST_LOG_INFO(String,Term),
    error_logger:info_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).

-define(SUPERCAST_LOG_INFO(String),
    error_logger:info_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).

-define(SUPERCAST_LOG_WARNING(String,Term),
    error_logger:warning_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).


-define(SUPERCAST_LOG_WARNING(String),
    error_logger:warning_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).
-else.
-define(SUPERCAST_LOG_WARNING(String), ok).
-define(SUPERCAST_LOG_WARNING(String,Term), ok).
-define(SUPERCAST_LOG_INFO(String), ok).
-define(SUPERCAST_LOG_INFO(String,Term), ok).
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
