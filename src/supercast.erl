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
%%% Suercast high level API.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(supercast).

-include("supercast.hrl").

%% API
-export([ %% start/stop
    start/0,
    listen/0,
    stop/0]).

-export([ %% convenience API
    new/4]).

-export([ %% users access control utils
    satisfy/2]).

-export_type([sc_message/0,sc_queryid/0,sc_reference/0]).
-type(sc_queryid() :: integer() | undefined).
-type(sc_message() :: jsx:json_term()).
-type(sc_reference() :: {
    Channel :: string(),
    CState  :: #client_state{},
    QueryId :: sc_queryid()}).

%%------------------------------------------------------------------------------
%% @equiv supercast_channel:new/4
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------
new(Name, Module, Args, Perm) ->
    supercast_channel:new(Name,Module,Args,Perm).


%%------------------------------------------------------------------------------
%% @doc
%% This is an debug utility automaticaly calling supercast:listen().
%%
%% @end
%%------------------------------------------------------------------------------
-spec(start() -> ok).
start() ->
    application:start(xmerl),
    application:start(supercast),
    supercast:listen().


%%------------------------------------------------------------------------------
%% @doc
%% Start listening for incomming client connexions.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(listen() -> ok).
listen() ->
    {ok, DocRoot} = application:get_env(supercast, http_docroot),
    DocrootPath = filename:absname(DocRoot),
    DocrootIndex = filename:join(DocrootPath, "index.html"),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", supercast_endpoint_websocket, []},
            {"/", cowboy_static, {file, DocrootIndex, [{etag,false}]}},
            {"/[...]", cowboy_static, {dir, DocrootPath, [{etag,false}]}}
        ]}
    ]),

    {ok, HTTPPort} = application:get_env(supercast, http_port),

    %% keep-alive header is not shown because it is implicit
    %% (HTTP1.0: close HTTP1.1: keep-alive)
    {ok, _} = cowboy:start_http(supercast_http, 10, [{port, HTTPPort}], [
        {env, [{dispatch, Dispatch}]},
        {max_keepalive, 50}
    ]),

    {ok, TCPPort} = application:get_env(supercast, tcp_port),
    {ok, _} = ranch:start_listener(supercast_tcp, 10, ranch_tcp,
        [{port, TCPPort}], supercast_endpoint_tcp, []),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Stop the supercast server.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(stop() -> ok).
stop() ->
    application:stop(supercast).


%%%=============================================================================
%%% User utils
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Return true if the user is allowed to read the ressource.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(satisfy(CState :: #client_state{}, Perm :: #perm_conf{}) -> true | false).
satisfy(CState, Perm) ->
    {ok, AccCtrl} = application:get_env(supercast, acctrl_module),
    case AccCtrl:satisfy(read, [CState], Perm) of
        {ok, []}        -> false;
        {ok, [CState]}  -> true
    end.

