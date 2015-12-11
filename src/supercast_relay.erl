
%% @private
-module(supercast_relay).
-behaviour(gen_server).
-include("supercast.hrl").

%% API
-export([start_link/1]).
-export([multicast/3, unicast/3]).

-export([subscribe/2, unsubscribe/1, unsubscribe/2, subscribe_stage2/2]).

%% called from supercast module
-export([delete/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-define(ENCODER, jsx).

-record(state, {
    chan_name,
    clients = []
}).

start_link(Name) ->
    gen_server:start_link({via, supercast_reg, Name}, ?MODULE, Name, []).

-spec subscribe(CState::#client_state{}, Channel::string()) -> ok | error.
%% @doc called from the socket
subscribe(CState, Channel) ->
    %% does the channel exist?
    case ets:lookup(?ETS_CHAN_STATES, Channel) of
        [] -> %% no
            error;
        [#chan_state{perm=Perm}] -> %% yes
            %% the client is allowed to connect to the channel?
            AcctrlMod = application:get_env(supercast, acctrl_module),
            case AcctrlMod:satisfy(read, [CState], Perm) of
                {ok, []} -> %% no
                    error;
                _ ->
                    %% create relay if it does not exists
                    %% start_child will return {ok,Pid} or {error,allready_tarted}
                    supercast_relay_sup:start_relay([Channel]),
                    subscribe_stage1(Channel, CState)
                    %% subscribe_stage1 will return immediately. The client side
                    %% is now waiting for sync and events pdus.
            end
    end.

-spec subscribe_stage1(Channel::string(), CState::#client_state{}) -> ok | error.
%% @doc called from supercast_endpoint to initialize a client subscription.
subscribe_stage1(Channel, CState) ->
    gen_server:cast({via, supercast_reg, Channel}, {subscribe_stage1, CState}).

-spec subscribe_stage2(Ref::reference(), Pdus::[term()]) -> ok.
%% @doc called from the supercast_channel process
subscribe_stage2(Ref, Pdus) ->
    RefList = ets_take(?ETS_SYN_STATES, Ref),
    case RefList of
        [] -> ok;
        [{_, Channel, CState}] ->
            %% effectively registered here
            gen_server:cast({via, supercast_reg, Channel},
                    {subscribe_stage2, CState, Pdus})
    end.

-spec unsubscribe(CState::#client_state{}) -> ok.
%% @doc unsubscribe the client from all channels
unsubscribe(CState) ->
    Chans = [Name || #chan_state{name=Name} <- ets:tab2list(?ETS_CHAN_STATES)],
    lists:foreach(fun(Chan) ->
        unsubscribe(CState, Chan)
    end, Chans).

-spec unsubscribe(CState::#client_state{}, Channel::string()) -> ok.
%% @doc unsubscribe the client from one channel.
unsubscribe(CState, Channel) ->
    gen_server:cast(Channel, {unsubscribe, CState}).

-spec delete(Channel::string()) -> ok.
delete(Channel) ->
    gen_server:cast({via, supercast_reg, Channel}, delete).

-spec multicast(Pid::pid(), Msgs::[supercast_msg()],
                                            Perm::#perm_conf{} | default) -> ok.
%% @doc Send messages to multiple clients. Default mean that there will be no
%% filtering. IE: All clients allowed to register to the channel will
%% receive the message.
%% @end
multicast(Pid, Msgs, Perm) ->
    gen_server:cast(Pid, {multicast, Perm, Msgs}).


-spec unicast(Pid::pid(),To::#client_state{},Msgs::[supercast_msg()]) -> ok.
%% @doc Send messages to an unique client.
unicast(Pid, To, Msgs) ->
    gen_server:cast(Pid, {unicast, To, Msgs}).


init([ChanName]) ->
    process_flag(trap_exit, true),
    case ets:lookup(?ETS_CHAN_STATES, ChanName) of
        [] ->
            %% channel has vanished
            {stop, "Channel has vanished"};
        _ ->
            %% Now the process can allready have in his queue a cast(delete)
            {ok, #state{chan_name=ChanName}}
    end.


handle_cast({multicast, default, Msgs},
                        #state{chan_name=_ChanName,clients=Clients} = State) ->
    multi_send(Clients, Msgs),
    {noreply, State};

handle_cast({multicast, Perm, Msgs},
                        #state{chan_name=_ChanName,clients=Clients} = State) ->
    {ok, Acctrl} = application:get_env(supercast, acctrl_module),
    Clients2 = Acctrl:satisfy(read, Clients, Perm),
    multi_send(Clients2, Msgs),
    {noreply, State};

handle_cast({unicast, #client_state{module=Mod} = To, Msgs},
                                        #state{chan_name=_ChanName} = State) ->
    lists:foreach(fun(P) ->
        Mod:send(To, ?ENCODER:encode(P))
    end, Msgs),
    {noreply, State};

handle_cast({unsubscribe, CState}, #state{clients=Clients} = State) ->
    case lists:delete(CState, Clients) of
        []   ->
            %% without more subscribers, the process will die in 2 seconds
            {noreply, State#state{clients=[]}, 2000};
        Rest ->
            {noreply, State#state{clients=Rest}}
    end;

handle_cast({subscribe_stage1, CState}, #state{chan_name=ChanName} = State) ->
    SynRef = {erlang:make_ref(), ChanName, CState},
    ets:insert(?ETS_SYN_STATES, SynRef),
    erlang:spawn(fun() ->
        case ets:lookup(?ETS_CHAN_STATES, ChanName) of
            [] ->
                %% channel has vanished but I have the information in my queue
                error;
            [#chan_state{module=Mod,args=Args}] ->
                _Ignore = Mod:syn(ChanName, Args, CState, SynRef)
        end
    end),
    {noreply, State};

handle_cast({subscribe_stage2, CState, Pdus}, #state{clients=CList} = State) ->

    #client_state{module=Mod} = CState,
    lists:foreach(fun(P) -> Mod:send(CState, ?ENCODER:encode(P)) end, Pdus),

    {noreply, State#state{clients = [CState|CList]}};

handle_cast(delete, #state{clients=Clients,chan_name=Name}) ->
    Pdu = ?ENCODER:encode(pdu(channelDeleted, Name)),
    lists:foreach(fun(#client_state{module=Mod} = C) ->
        Mod:send(C, Pdu)
    end, Clients),
    {stop, {"Channel deleted", Name}};

handle_cast(_Request, State) -> {noreply, State}.

terminate(_Reason, #state{chan_name=Name}) ->
    supercast_reg:unregister_name(Name),
    ok.

handle_call(_Request, _From, State) -> {noreply, State}.
handle_info(timeout, #state{clients=[]})  -> {stop, "No more clients"};
handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


pdu(channelDeleted, Channel) ->
        [
            {<<"from">>, <<"supercast">>},
            {<<"type">>, <<"channelDeleted">>},
            {<<"value">>, [
                {<<"channel">>, list_to_binary(Channel)}]
            }
        ].

multi_send(Clients, Msgs) ->
    lists:foreach(fun(Message) ->
        Pdu = ?ENCODER:encode(Message),
        lists:foreach(fun(#client_state{module=Mod} = Client) ->
            Mod:send(Client, Pdu)
        end, Clients)
    end, Msgs).

ets_take(Table, Key) ->
    Val = ets:lookup(Table, Key),
    ets:delete(Table, Key),
    Val.
