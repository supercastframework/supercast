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
%%% @end
%%% @TODO another file to avoid dependencie loop
%%%-----------------------------------------------------------------------------
-module(supercast_channel).
-behaviour(gen_server).
-include("supercast.hrl").

%% API
-export([
    start_link/4,
    new/4]).

%% supercast_endpoint calls
-export([
    join/4,
    leave/4]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    channel :: string(),
    module  :: atom(),
    perm    :: #perm_conf{},
    opaque  :: term()
}).


-callback(supercast_init(Channel :: string(), Args :: term()) ->
    {ok, State :: term()} |
    {stop, Reason :: normal | shutdown | term()}).
-callback(supercast_join(Channel :: string(), CState :: #client_state{},
        State :: term()) ->
    {accept, NewState :: term()} |
    {accept, Pdus :: supercast:sc_message(), NewState :: term()} |
    {refuse, NewState :: term()} |
    {stop, Reason :: normal | shutdown | term()}).
-callback(supercast_leave(Channel :: string(), CState :: #client_state{},
        State :: term()) ->
    {ok, NewState :: term()} |
    {ok, Pdus :: supercast:sc_message(), NewState :: term()} |
    {stop, Reason :: normal | shutdown | term()}).
-callback(supercast_info(Channel :: string(), Info :: term(),
        State :: term()) ->
    {ok, NewState :: term()} |
    {stop, Reason :: normal | shutdown | term()}).
-callback(supercast_close(Channel :: string(), term(), State :: term()) ->
    Ignored :: term()).


%%%=============================================================================
%%% supervisor API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec start_link(Channel :: string(), Module :: atom(), Args :: term(),
    Perm :: #perm_conf{}) -> {ok,    Pid :: pid()} |{error, Reason :: term()}.
start_link(Channel, Module, Args, Perm) ->
    gen_server:start_link(?MODULE, {Channel, Module, Args, Perm}, []).


%%%=============================================================================
%%% supervisor API
%%%=============================================================================


%%%=============================================================================
%%% supercast_endpoints API
%%%=============================================================================
new(Channel, Module, Args, Perm) ->
    supercast_channel_sup:new_channel([Channel, Module, Args, Perm]).


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec join(Channel :: string(), Self :: pid(), CState :: #client_state{},
    Ref :: supercast:sc_reference()) -> ok.
join(Channel, _Args = Self, CState, Ref) ->
    gen_server:cast(Self, {join, Channel, CState, Ref}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec leave(Channel :: string(), Self :: pid(), CState :: #client_state{},
    Ref :: supercast:sc_reference()) -> ok.
leave(Channel, _Args = Self, CState, Ref) ->
    gen_server:cast(Self, {leave, Channel, CState, Ref}).



%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec init({Channel :: string(), Module :: atom(), Args :: term(),
    Perm :: #perm_conf{}}) -> {ok, State :: #state{}}.
init({Channel, Module, Args, Perm}) ->
    process_flag(trap_exit, true),
    supercast:new(Channel, ?MODULE, self(), Perm),
    case Module:init_channel(Channel, Args) of
        {stop, Reason} ->
            {stop, Reason};
        {ok, Opaque} ->
            {ok, #state{module=Module,opaque=Opaque}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) -> {reply, ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({join, Channel, CState, Ref},
                                    #state{module=Mod,opaque=Opaque} = State) ->
    case Mod:supercast_join(Channel, CState, Opaque) of
        {accept, Opaque2} ->
            supercast:join_accept(Ref),
            {noreply, State#state{opaque=Opaque2}};
        {accept, Pdus, Opaque2} ->
            supercast:join_accept(Ref, Pdus),
            {noreply, State#state{opaque=Opaque2}};
        {refuse, Opaque2} ->
            supercast:join_refuse(Ref),
            {noreply, State#state{opaque=Opaque2}};
        {stop, Reason} ->
            supercast:join_refuse(Ref),
            {stop, Reason};
        _R ->
            supercast:join_refuse(Ref),
            {stop, bad_return, {_R, State}}
    end;

handle_cast({leave, Channel, CState, Ref},
                                    #state{module=Mod,opaque=Opaque} = State) ->
    case Mod:supercast_leave(Channel, CState, Opaque) of
        {ok, Opaque2} ->
            supercast:leave_ack(Ref),
            {noreply, State#state{opaque=Opaque2}};
        {ok, Pdus, Opaque2} ->
            supercast:leave_ack(Ref, Pdus),
            {noreply, State#state{opaque=Opaque2}};
        {stop, Reason} ->
            supercast:leave_ack(Ref),
            {stop, Reason};
        _R ->
            supercast:leave_ack(Ref),
            {stop, bad_return, {_R, State}}
    end;

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
handle_info(Info, #state{module=Mod, opaque=Opaque} = State) ->
    {ok, Opaque2} = Mod:info(Info, Opaque),
    {noreply, State#state{opaque=Opaque2}}.

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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
