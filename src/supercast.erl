%%------------------------------------------------------------------------------
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
%%------------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% @author Sebastien Serre <ssbx@supercastframework.org>
%%% @copyright (C) 2015, Sebastien Serre
%%% @doc
%%% The main supercast API module.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(supercast).
-behaviour(gen_server).

-include("supercast.hrl").

%% API
-export([ %% start/stop
    start/0,
    start_link/0,
    listen/0,
    stop/0]).

-export([ %% main user API
    new_channel/4,
    delete_channel/1,
    unicast/3,
    multicast/3,
    broadcast/2,
    join_accept/2,
    join_accept/1,
    join_refuse/1]).

-export([ %% other user utils
    filter/2,
    satisfy/2]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% pid registry behaviour
%-export([
    %register_name/2,
    %unregister_name/1,
    %whereis_name/1,
    %send/2]).

%%%=============================================================================
%%% Start/Stop API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @see supercast:start_link/0
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
%% @see gen_server:start_link/4
%% @see supercast:listen/0
%% @doc
%% Starts the server. You will need to call supercast:listen/0 when your
%% application initialisation is complete to start listen for
%% clients connections.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link(?MODULE, ?MODULE, [], []).


%%------------------------------------------------------------------------------
%% @see supercast:start_link/0
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
%%% User API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @see supercast_channel
%% @doc
%% Register a new channel.
%%
%% <em>ChanName</em> is the name of the channel.
%%
%% <em>Module</em> is the name of the module implementing the supercast_channel
%% behaviour.
%%
%% <em>Opts</em> is any term passed to supercast_channel callbacks.
%% @see supercast_channel:join/3
%% @see supercast_channel:leave/1
%%
%% <em>Perm</em> is the permissions of the channel. Only read is handled by
%% supercast.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(new_channel(ChanName :: string(), Module :: atom(), Opts :: any(),
        Perm :: #perm_conf{}) -> ok).
new_channel(ChanName, Module, Args, Perm) ->
    ets:insert(?ETS_CHAN_STATES,
        #chan_state{name=ChanName,module=Module,perm=Perm,args=Args}),
    ok.


%%------------------------------------------------------------------------------
%% @doc
%% Delete a registered channel.
%%
%% This function will send a <em>channelVanished</em> message to all connected
%% clients.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(delete_channel(ChannName :: string()) -> ok).
delete_channel(ChanName) ->
    ets:delete(?ETS_CHAN_STATES, ChanName),
    case supercast_relay_register:whereis_name(ChanName) of
        undefined -> ok;
        Pid       -> supercast_relay:delete(Pid)
    end.


%%------------------------------------------------------------------------------
%% @doc
%% Send messages to a client wherever he have joined the channel or not.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(unicast(Channel :: string(), Messages :: [supercast_msg()],
    To :: #client_state{}) -> ok).
unicast(Channel, Messages, To) ->
    case supercast_relay_register:whereis_name(Channel) of
        undefined -> ok;
        Pid       -> supercast_relay:unicast(Pid, Messages, To)
    end.


%%------------------------------------------------------------------------------
%% @doc
%% Send messages to all clients of the specified channel, wich satisfy with the
%% <em>read</em> permission of the #perm_conf{}.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(multicast(Channel :: string(), Messages :: [supercast_msg()],
                                CustomPerm :: default | #perm_conf{}) -> ok).
multicast(Channel, Messages, Perm) ->
    case supercast_relay_register:whereis_name(Channel) of
        undefined -> ok;
        Pid       -> supercast_relay:multicast(Pid, Messages, Perm)
    end.


%%------------------------------------------------------------------------------
%% @doc
%% Send messages to all clients of the specified channel.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(broadcast(Channel::string(), Message::[supercast_msg()]) -> ok).
%% @equiv multicast(Channel, Messages, default).
broadcast(Channel, Message) ->
    multicast(Channel, Message, default).


%%------------------------------------------------------------------------------
%% @equiv join_ack(Ref, [])..
%%------------------------------------------------------------------------------
-spec(join_accept(Ref :: {Channel :: string(), CState :: #client_state{},
    QueryId :: integer}) -> ok).
join_accept(Ref) -> join_accept(Ref, []).


%%------------------------------------------------------------------------------
%% @see join_del/1
%% @doc
%% Must be called from <em>supercast_channel:join/3</em> to effectively
%% subscribe the client to the channel.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(join_accept(Ref :: {Channel :: string(), CState :: #client_state{},
    QueryId :: integer}, Pdus :: [term()]) -> ok).
join_accept({Channel, CState, QueryId}, Pdus) ->
    supercast_relay:subscribe_ack(Channel, CState, QueryId, Pdus).


%%------------------------------------------------------------------------------
%% @see join_ack/2
%% @doc
%% Must be called from <em>supercast_channel:join/3</em> to cancel the user
%% request to join the channel. It notify the client with  a
%% <em>subscribeErr</em> message.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(join_refuse(Ref :: {Channel :: string(), CState :: #client_state{},
    QueryId :: integer}) -> ok).
join_refuse({Channel, #client_state{module=Mod} = CState, QueryId}) ->
    ErrPdu = supercast_endpoint:pdu(subscribeErr, {QueryId, Channel}),
    Mod:send(CState, ErrPdu).




%%%=============================================================================
%%% User utils
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Return true if the user is allowed to read the ressource.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(satisfy(CState :: #client_state{}, Perm :: tuple()) -> true | false).
satisfy(CState, Perm) ->
    {ok, AccCtrl} = application:get_env(supercast, acctrl_module),
    case AccCtrl:satisfy(read, [CState], Perm) of
        {ok, []}        -> false;
        {ok, [CState]}  -> true
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Called by external modules to filter things. It will return any "things",
%% that the client defined in #client_state{} is allowed to 'read'.
%%
%% @end
%%------------------------------------------------------------------------------
-spec(filter(CState :: #client_state{}, Values :: [{#perm_conf{},
        Things :: term()}]) -> [Things :: term()]).
filter(CState, Values) ->
    filter_things(CState, Values, []).

%%------------------------------------------------------------------------------
%% @see filter/2
%%------------------------------------------------------------------------------
-spec(filter_things(CState :: #client_state{}, Values :: [{#perm_conf{},
        Things :: term()}], Acc :: list()) -> [Things :: term()]).
filter_things(_, [], Acc) -> Acc;
filter_things(CState, [{Perm, Thing}|T], Acc) ->
    case satisfy([CState], Perm) of
        false -> filter_things(CState, T, Acc);
        true  -> filter_things(CState, T, [Thing|Acc])
    end.



%%%=============================================================================
%%% Pid registry behaviour callbacks
%%%=============================================================================



%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-record(state, {}).

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.


%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%------------------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%------------------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%------------------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.