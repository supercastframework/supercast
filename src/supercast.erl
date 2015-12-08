%% -------------------------------------------------------------------
%% Supercast Copyright (c) 2012-2015 Sebastien Serre <ssbx@sysmo.io>
%% All Rights Reserved.
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

-module(supercast).
-include("supercast.hrl").

-export([listen/0]).
-export([filter/2, satisfy/2, mpd_state/0]).
-export([start/0,stop/0]).

-spec start() -> ok.
%% @doc Start the supercast server.
start() ->
    ensure_started(xmerl),
    application:start(supercast),
    supercast:listen().

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

-spec stop() -> ok.
%% @doc Stop the supercast server.
stop() ->
    application:stop(supercast).

-spec listen() -> ok.
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
    {ok, _} = cowboy:start_http(supercast_http, 50, [{port, HTTPPort}], [
        {env, [{dispatch, Dispatch}]},
        {max_keepalive, 50}
    ]),

    {ok, TCPPort} = application:get_env(supercast, tcp_port),
    {ok, _} = ranch:start_listener(supercast_tcp, 10, ranch_tcp,
        [{port, TCPPort}], supercast_endpoint_tcp, []),
    ok.

-spec satisfy(CState::#client_state{}, Perm::tuple()) -> true | false.
%% @doc Return true if the user is allowed to read the ressource.
satisfy(CState, Perm) ->
    {ok, AccCtrl} = application:get_env(supercast, acctrl_module),
    case AccCtrl:satisfy(read, [CState], Perm) of
        {ok, []}        -> false;
        {ok, [CState]}  -> true
    end.

-spec filter(CState::#client_state{}, Values::[{#perm_conf{},
        Things::term()}]) -> [Things::term()].
%% @doc Called by external modules to filter things. It will return any "things",
%% that the client defined in #client_state{} is allowed to 'read'.
filter(CState, Values) ->
    filter_things(CState, Values, []).

filter_things(_, [], R) ->
    R;
filter_things(CState, [{Perm, Thing}|T], R) ->
    case satisfy([CState], Perm) of
        false ->
            filter_things(CState, T, R);
        true  ->
            filter_things(CState, T, [Thing|R])
    end.

%% @private
-spec mpd_state() -> {ok, tuple()}.
%% @doc This function return the state of the supercast_mpd gen_server module.
mpd_state() ->
    gen_server:call(supercast_mpd, dump).
