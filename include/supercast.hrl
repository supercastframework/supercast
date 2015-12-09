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

-type   supercast_msg()     ::  {function, fun()} | {pdu, tuple()}.
-record(perm_conf, {
    read    = []    :: [term()],
    write   = []    :: [term()]
}).

-record(registered_chan, {
    name,
    pid,
    module
}).

-record(chan, {
    id          = undefined :: atom(),
    perm        = undefined :: undefined | #perm_conf{}
}).

-record(client_state,  {
    socket,                     % client socket
    addr,                       % client address
    port,                       % client port
    certificate,                % ssl certificate
    ca_certificate,             % for self signed certs
    key,                        % ssl key
    ref,                        % reference ovoiding socket swap in the 
                                % middle of a async call
    user_name = [],             % user attached to the socket
    user_roles = [],            % groups wich the user belong
    user_modules,               % modules allowed at client connexion
    auth_request_count = 1,     % used by max request count
    module,                     % callback mod to send data
    encoding_mod,               %
    communication_mod,          % must implement the X:send(Socket, Msg) fun
    authenticated,              % boolean
    ranch_transport,
    ranch_ref,
    pid                         % pid() of the gen_server howner of the socket
}). 

-record(supercast_module, {
    name        = undefined :: atom(),
    callback    = undefined :: module(),
    asnkey      = undefined :: atom(),
    static_chan = undefined :: pid() | reference(),
    perm        = []        :: [string()]
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
