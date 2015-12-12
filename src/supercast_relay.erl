
%% @private
-module(supercast_relay).
-behaviour(gen_server).
-include("supercast.hrl").

%% API
-export([start_link/1]).
-export([multicast/3, unicast/3]).

-export([subscribe/3, unsubscribe/1, unsubscribe/2]).

%% called from supercast module
-export([delete/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-record(state, {
    chan_name,
    clients = []
}).

start_link(Name) ->
    gen_server:start_link({via, supercast_relay_register, Name}, ?MODULE, Name, []).

-spec subscribe(CState :: #client_state{}, Channel :: string(),
    QueryId :: integer()) -> ok | error.
%% @doc called from the socket
subscribe(CState, Channel, QueryId) ->
    %% does the channel exist?
    ?SUPERCAST_LOG_INFO("subscribe", {CState, Channel}),
    case ets:lookup(?ETS_CHAN_STATES, Channel) of
        [] -> %% no
            ?SUPERCAST_LOG_INFO("no channel"),
            error;
        [#chan_state{perm=Perm}] -> %% yes
            ?SUPERCAST_LOG_INFO("yes channel"),
            %% the client is allowed to connect to the channel?
            {ok, AcctrlMod} = application:get_env(supercast, acctrl_module),
            ?SUPERCAST_LOG_INFO("acctrl", {AcctrlMod, Perm}),
            case AcctrlMod:satisfy(read, [CState], Perm) of
                {ok, []} -> %% no
                    ?SUPERCAST_LOG_INFO("does not satisfy"),
                    error;
                _ ->
                    ?SUPERCAST_LOG_INFO("does satisfy"),
                    %% create relay if it does not exists
                    %% start_child will return {ok,Pid} or {error,allready_tarted}
                    supercast_relay_sup:start_relay([Channel]),
                    %% subscribe_final will return immediately. The client side
                    %% is now waiting for ack, sync and events pdus.
                    gen_server:cast({via, supercast_relay_register, Channel},
                        {subscribe, QueryId, CState})
            end
    end.

-spec unsubscribe(CState::#client_state{}) -> ok.
%% @doc unsubscribe the client from all channels
unsubscribe(CState) ->
    ?SUPERCAST_LOG_INFO("unsubscribe all", CState),
    Chans = [Name || #chan_state{name=Name} <- ets:tab2list(?ETS_CHAN_STATES)],
    lists:foreach(fun(Chan) ->
        unsubscribe(CState, Chan)
    end, Chans).

-spec unsubscribe(Channel::string(), CState::#client_state{}) -> ok.
%% @doc unsubscribe the client from one channel.
unsubscribe(Channel, CState) ->
    ?SUPERCAST_LOG_INFO("unsubscribe chan", {Channel, CState}),
    gen_server:cast({via, supercast_relay_register, Channel}, {unsubscribe, CState}).

-spec delete(Channel::string()) -> ok.
delete(Channel) ->
    ?SUPERCAST_LOG_INFO("delete channel", Channel),
    gen_server:cast({via, supercast_relay_register, Channel}, delete).

-spec multicast(Pid::pid(), Msgs::[supercast_msg()],
                                            Perm::#perm_conf{} | default) -> ok.
%% @doc Send messages to multiple clients. Default mean that there will be no
%% filtering. IE: All clients allowed to register to the channel will
%% receive the message.
%% @end
multicast(Pid, Msgs, Perm) ->
    ?SUPERCAST_LOG_INFO("multicast", {Pid,Msgs,Perm}),
    gen_server:cast(Pid, {multicast, Msgs, Perm}).


-spec unicast(Pid::pid(), Msgs::[supercast_msg()], To::#client_state{}) -> ok.
%% @doc Send messages to an unique client.
unicast(Pid, Msgs, To) ->
    ?SUPERCAST_LOG_INFO("unicast", {Pid,Msgs,To}),
    gen_server:cast(Pid, {unicast, Msgs, To}).


init(ChanName) ->
    process_flag(trap_exit, true),
    ?SUPERCAST_LOG_INFO("init channel", ChanName),
    case ets:lookup(?ETS_CHAN_STATES, ChanName) of
        [] ->
            ?SUPERCAST_LOG_INFO("channel vanished", ChanName),
            %% channel has vanished
            {stop, "Channel has vanished"};
        _ ->
            ?SUPERCAST_LOG_INFO("channel continue", ChanName),
            %% Now the process can allready have in his queue a cast(delete)
            {ok, #state{chan_name=ChanName}}
    end.


handle_cast({multicast, Msgs, default},
                        #state{chan_name=_ChanName,clients=Clients} = State) ->
    ?SUPERCAST_LOG_INFO("multicast"),
    multi_send(Clients, Msgs),
    {noreply, State};

handle_cast({multicast, Msgs, Perm},
                        #state{chan_name=_ChanName,clients=Clients} = State) ->
    ?SUPERCAST_LOG_INFO("multicast"),
    {ok, Acctrl} = application:get_env(supercast, acctrl_module),
    Clients2 = Acctrl:satisfy(read, Clients, Perm),
    multi_send(Clients2, Msgs),
    {noreply, State};

handle_cast({unicast, Msgs, #client_state{module=Mod} = To},
                                        #state{chan_name=_ChanName} = State) ->
    ?SUPERCAST_LOG_INFO("unicast"),
    lists:foreach(fun(P) ->
        Mod:send(To, P)
    end, Msgs),
    {noreply, State};

handle_cast({unsubscribe, CState}, #state{clients=Clients} = State) ->
    ?SUPERCAST_LOG_INFO("unsubscribe"),
    case lists:delete(CState, Clients) of
        []   ->
            %% without more subscribers, the process will die in 10 seconds
            {noreply, State#state{clients=[]}, 10000};
        Rest ->
            {noreply, State#state{clients=Rest}}
    end;

handle_cast({subscribe, QueryId, #client_state{module=Mod} = CState},
                #state{chan_name=ChanName,clients=Clients} = State) ->
    ?SUPERCAST_LOG_INFO("subscribe cast"),
    case lists:member(CState, Clients) of

        false ->
            ?SUPERCAST_LOG_INFO("false"),

            case ets:lookup(?ETS_CHAN_STATES, ChanName) of

                [#chan_state{module=CMod,args=Args}] ->
                    ?SUPERCAST_LOG_INFO("found in chan_states"),

                    case CMod:join(ChanName, Args, CState) of
                        {ok, Pdus} ->
                            OkPdu = supercast_endpoint:pdu(
                                            subscribeOk, {QueryId, ChanName}),
                            lists:foreach(fun(P) ->
                                Mod:send(CState, P)
                            end, [OkPdu|Pdus]),
                            {noreply, State#state{clients=[CState|Clients]}};

                        ok ->
                            {noreply, State#state{clients=[CState|Clients]}};

                        _Err ->
                            ?SUPERCAST_LOG_INFO("error: ", _Err),
                            ErrPdu = supercast_endpoint:pdu(
                                subscribeErr, {QueryId, ChanName}),
                            Mod:send(CState, ErrPdu),
                            {noreply, State}

                    end;
                _Other ->
                    ?SUPERCAST_LOG_INFO("other", _Other),
                    ErrPdu = supercast_endpoint:pdu(
                                            subscribeErr, {QueryId, ChanName}),
                    Mod:send(CState, ErrPdu),
                    {noreply, State}

            end;

        true -> %% allready registered
            ?SUPERCAST_LOG_INFO("true"),
            OkPdu = supercast_endpoint:pdu(subscribeOk, {QueryId, ChanName}),
            Mod:send(CState, OkPdu),
            {noreply, State}
    end;

handle_cast(delete, #state{clients=Clients,chan_name=Name}) ->
    ?SUPERCAST_LOG_INFO("delete channel"),
    Pdu = ?ENCODER:encode(pdu(channelDeleted, Name)),
    lists:foreach(fun(#client_state{module=Mod} = C) ->
        Mod:raw_send(C, Pdu)
    end, Clients),
    {stop, {"Channel deleted", Name}};

handle_cast(_Cast, State) ->
    ?SUPERCAST_LOG_INFO("unknown cast", _Cast),
    {noreply, State}.

terminate(_Reason, #state{chan_name=Name}) ->
    ?SUPERCAST_LOG_INFO("terminate relay", _Reason),
    supercast_relay_register:unregister_name(Name),
    ok.

handle_call(_Request, _From, State) -> {noreply, State}.
handle_info(timeout, #state{clients=[]}) -> {stop, normal, #state{}};
handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


pdu(channelDeleted, Channel) ->
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"channelDeleted">>},
            {<<"value">>, [
                {<<"channel">>, list_to_binary(Channel)}
            ]}
        ].

multi_send(Clients, Msgs) ->
    lists:foreach(fun(Message) ->
        Pdu = ?ENCODER:encode(Message),
        lists:foreach(fun(#client_state{module=Mod} = Client) ->
            Mod:raw_send(Client, Pdu)
        end, Clients)
    end, Msgs).

