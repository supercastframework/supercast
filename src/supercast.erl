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

-module(supercast).
-include("supercast.hrl").

%% API
-export([new_channel/4,delete_channel/1,unicast/3,
            multicast/3,broadcast/2,join_ack/2,join_ack/1,join_del/1]).

%% utils
-export([filter/2, satisfy/2]).
-export([start/0,listen/0,stop/0]).

-spec new_channel(ChanName::string(), Module::atom(), Opts::any(),
                                                    Perm::#perm_conf{}) -> ok.
new_channel(ChanName, Module, Args, Perm) ->
    ets:insert(?ETS_CHAN_STATES,
        #chan_state{name=ChanName,module=Module,perm=Perm,args=Args}),
    ok.

-spec delete_channel(ChannName::string()) -> ok.
delete_channel(ChanName) ->
    ets:delete(?ETS_CHAN_STATES, ChanName),
    case supercast_relay_register:whereis_name(ChanName) of
        undefined -> ok;
        Pid       -> supercast_relay:delete(Pid)
    end.


-spec unicast(Channel::string(),Messages::[supercast_msg()],
    To::#client_state{}) -> ok.
unicast(Channel, Messages, To) ->
    case supercast_relay_register:whereis_name(Channel) of
        undefined -> ok;
        Pid       -> supercast_relay:unicast(Pid, Messages, To)
    end.


-spec multicast(Channel::string(), Messages::[supercast_msg()],
                                CustomPerm::default | #perm_conf{}) -> ok.
multicast(Channel, Messages, Perm) ->
    case supercast_relay_register:whereis_name(Channel) of
        undefined -> ok;
        Pid       -> supercast_relay:multicast(Pid, Messages, Perm)
    end.

-spec broadcast(Channel::string(), Message::[supercast_msg()]) -> ok.
%% @equiv multicast(Channel, Messages, default).
broadcast(Channel, Message) ->
    multicast(Channel, Message, default).


%% @equiv join_ack(Ref, []).
%% @see join_del/1
-spec(join_ack(Ref :: {Channel :: string(), CState :: #client_state{},
    QueryId :: integer}) -> ok).
join_ack(Ref) -> join_ack(Ref, []).

%% @see join_del/1
-spec(join_ack(Ref :: {Channel :: string(), CState :: #client_state{},
    QueryId :: integer}, Pdus :: [term()]) -> ok).
join_ack({Channel, CState, QueryId}, Pdus) ->
    supercast_relay:subscribe_ack(Channel, CState, QueryId, Pdus).

%% @see join_ack/2
-spec(join_del(Ref :: {Channel :: string(), CState :: #client_state{},
    QueryId :: integer}) -> ok).
join_del({Channel, #client_state{module=Mod} = CState, QueryId}) ->
    ErrPdu = supercast_endpoint:pdu(subscribeErr, {QueryId, Channel}),
    Mod:send(CState, ErrPdu).






-spec start() -> ok.
%% @doc Localy start the supercast server.
%% This is an utility function, wich automaticaly call supercast:listen().
%% You should embed supercast in your application.
%% @end
start() ->
    application:start(xmerl),
    application:start(supercast),
    supercast:listen().


-spec stop() -> ok.
%% @doc Stop the supercast server.
stop() ->
    application:stop(supercast).

-spec listen() -> ok.
%% @doc start listening for client connexion.
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
