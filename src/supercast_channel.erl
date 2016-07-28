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
%%% Most important module of supercast.
%%% @TODO document.
%%% @end
%%%
%%%-----------------------------------------------------------------------------
-module(supercast_channel).
-behaviour(gen_server).
-behaviour(supercast_proc).
-include("supercast.hrl").

%% API
-export([
    start_link/4,
    new/4]).

%% supcercast_proc callbacks
-export([
    join_request/4,
    leave_request/4,
    info_request/3]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    channel::string(),
    module::atom(),
    perm::#perm_conf{},
    opaque::term(),
    pending::[#client_state{}]
}).


-callback(channel_init(Channel :: string(), Args :: term()) ->
    {ok, State :: term()} |
    {stop, Reason :: normal | shutdown | term()}).
-callback(channel_join(Channel :: string(), CState :: #client_state{},
    State :: term()) -> {ok, NewState :: term()} |
    {ok, Pdus :: supercast:sc_message(), NewState :: term()} |
    {refuse, NewState :: term()} |
    {stop, Reason :: normal | shutdown | term()}).
-callback(channel_leave(Channel :: string(), CState :: #client_state{},
    State :: term()) -> {ok, NewState :: term()} |
    {ok, Pdus :: supercast:sc_message(), NewState :: term()} |
    {stop, Reason :: normal | shutdown | term()}).
-callback(channel_info(Channel :: string(), Info :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {ok, Pdus :: supercast:sc_message(), NewState :: term()} |
    {stop, Reason :: normal | shutdown | term()}).
-callback(channel_terminate(Channel :: string(), Reason :: term(),
    State :: term()) -> {ok, Pdus :: [supercast:sc_message()]} | term()).



%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec start_link(Channel :: string(), Module :: atom(), Args :: term(),
    Perm :: #perm_conf{}) -> {ok,    Pid :: pid()} |{error, Reason :: term()}.
start_link(Channel, Module, Args, Perm) ->
    gen_server:start_link(?MODULE, [Channel, Module, Args, Perm], []).


%%%=============================================================================
%%% public API
%%%=============================================================================
new(Channel, Module, Args, Perm) ->
    supercast_channel_sup:new_channel([Channel, Module, Args, Perm]).



%%%=============================================================================
%%% supercast_proc behaviour callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @see supercast_proc:join_request/4
%% @private
%%------------------------------------------------------------------------------
-spec join_request(Channel :: string(), Self :: pid(), CState :: #client_state{},
    Ref :: supercast:sc_reference()) -> ok.
join_request(_Channel, _Args = Self, CState, Ref) ->
    %% first we call to be sure the endpoint will be notified of a channel
    %% terminate. If it fails, an exception is catched and handled correctly
    %% by the endpoing process.
    Timeout = 5000,
    gen_server:call(Self, {join, CState, Ref}, Timeout).

%%------------------------------------------------------------------------------
%% @see supercast_proc:leave_request/4
%% @private
%%------------------------------------------------------------------------------
-spec leave_request(Channel :: string(), Self :: pid(), CState :: #client_state{},
    Ref :: supercast:sc_reference()) -> ok.
leave_request(_Channel, _Args = Self, CState, Ref) ->
    gen_server:cast(Self, {leave, CState, Ref}).

%%------------------------------------------------------------------------------
%% @see supercast_proc:info_request/4
%% @private
%%------------------------------------------------------------------------------
-spec info_request(Channel :: string(), Self :: pid(), Request :: term()) -> ok.
info_request(_Channel, _Args = Self, Request) ->
    gen_server:cast(Self, {info, Request}).



%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec init({Channel::string(), Module::atom(), Args::term(),
            Perm::#perm_conf{}}) -> {ok, State::#state{}} | {stop, term()}.
init([Channel, Module, Args, Perm]) ->
    process_flag(trap_exit, true),
    supercast_proc:new_channel(Channel, ?MODULE, self(), Perm),
    case Module:channel_init(Channel, Args) of
        {stop, Reason} ->
            {stop, Reason};
        {ok, Opaque} ->
            {ok, #state{channel=Channel,module=Module,opaque=Opaque,pending=[]}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) -> {reply, ok, #state{}}.
handle_call({join, CState, Ref}, _From, #state{pending=Pending} = State) ->
    gen_server:cast(self(), {join, CState, Ref}),
    {reply, ok, State#state{pending=[CState|Pending]}};
handle_call(_Request, _From, State) -> {reply, ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({join, CState, Ref},
    #state{channel=Name,module=Mod,opaque=Opaque,pending=Pending} = State) ->
    NewPending = lists:delete(CState, Pending),
    case Mod:channel_join(Name, CState, Opaque) of
        {ok, Opaque2} ->
            ?traceInfo("trace", Opaque2),
            supercast_proc:join_accept(Ref),
            {noreply, State#state{opaque=Opaque2,pending=NewPending}};
        {ok, Pdus, Opaque2} ->
            ?traceInfo("trace", Opaque2),
            supercast_proc:join_accept(Ref, Pdus),
            {noreply, State#state{opaque=Opaque2,pending=NewPending}};
        {refuse, Opaque2} ->
            ?traceInfo("trace", Opaque2),
            supercast_proc:join_refuse(Ref),
            {noreply, State#state{opaque=Opaque2,pending=NewPending}};
        {stop, Reason, Opaque2} ->
            ?traceInfo("trace", Reason),
            supercast_proc:join_refuse(Ref),
            {stop, Reason, State#state{opaque=Opaque2,pending=NewPending}};
        R ->
            ?traceInfo("trace", R),
            supercast_proc:join_refuse(Ref),
            {stop, {bad_return, R}, State#state{pending=NewPending}}
    end;

handle_cast({leave, CState, Ref},
    #state{channel=Name,module=Mod,opaque=Opaque} = State) ->
    case Mod:channel_leave(Name, CState, Opaque) of
        {ok, Opaque2} ->
            supercast_proc:leave_ack(Ref),
            {noreply, State#state{opaque=Opaque2}};
        {ok, Pdus, Opaque2} ->
            supercast_proc:leave_ack(Ref, Pdus),
            {noreply, State#state{opaque=Opaque2}};
        {stop, Reason, Opaque2} ->
            supercast_proc:leave_ack(Ref),
            {stop, Reason, State#state{opaque=Opaque2}};
        R ->
            supercast_proc:leave_ack(Ref),
            {stop, {bad_return, R}, State}
    end;

handle_cast({info, Request},
        #state{channel=Name,module=Mod,opaque=Opaque} = State) ->
    case Mod:channel_info(Name, Request, Opaque) of
        {ok, Opaque2} ->
            {noreply, State#state{opaque=Opaque2}};
        {stop, Reason, Opaque2} ->
            {stop, Reason, State#state{opaque=Opaque2}};
        R ->
            {stop, {bad_return, R}, State}

    end;

handle_cast(_Request, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(Info, #state{module=Mod, opaque=Opaque} = State) ->
    {ok, Opaque2} = Mod:channel_info(Info, Opaque),
    {noreply, State#state{opaque=Opaque2}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason,
        #state{channel=Channel,module=Mod,opaque=Opaque,pending=Pending}) ->
    case Mod:channel_terminate(Channel, Reason, Opaque) of
        {ok, Pdus} ->
            supercast_proc:send_broadcast(Channel, Pdus);
        _ -> ok
    end,

    %% handle pending endpoints
    ErrPdu = supercast_endpoint:pdu(subscribeErr, Channel),
    lists:foreach(fun(C) ->
        supercast_proc:send_unicast(C, [ErrPdu])
    end, Pending),

    %% delete the channel.
    supercast_proc:delete_channel(Channel).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) -> {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
