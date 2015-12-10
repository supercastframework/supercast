
%% @private
-module(supercast_relay).
-behaviour(gen_server).
-include("supercast.hrl").

%% API
-export([start_link/1]).
-export([broadcast/3, unicast/3]).

%% called from supercast_endpoint module
-export([subscribe/2]).

%% called from supercast module
-export([delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-record(state, {
    chan_name,
    clients
}).

start_link(ChanName) ->
    gen_server:start_link({via, supercast_reg, ChanName},
                                        ?MODULE, ChanName, []).

-spec subscribe(CState::#client_state{}, Channel::string()) -> ok | error.
subscribe(CState, Channel) ->
    %% does the channel exist?
    case ets:lookup(?ETS_CHAN_STATES, Channel) of
        [] -> %% no
            error;
        [#chan_state{perm=Perm}] -> %% yes
            %% the client is allowed to connect to the channel?
            AuthMod = application:get_env(supercast, auth_mod),
            case AuthMod:satisfy(read, [CState], Perm) of
                {ok, []} -> %% no
                    error;
                _ -> %% yes
                    %% does a supercast_relay allready exists?
                    case supercast_reg:whereis_name(Channel) of
                        undefined -> %% no
                            supercast_relay_sup:start_child([Channel,CState]),
                            subscribe2(Channel, CState),
                            ok;
                        _ -> %% yes
                            subscribe2(Channel, CState)
                    end
            end
    end.

-spec subscribe2(Channel::string(), CState::#client_state{}) -> ok | error.
subscribe2(Channel, CState) ->
    gen_server:call({via, supercast_reg, Channel}, {subscribe2, CState}).

-spec delete(Channel::string()) -> ok.
delete(Channel) ->
    gen_server:cast({via, supercast_reg, Channel}, delete).

-spec broadcast(Pid::pid(), Msgs::[supercast_msg()],
                                            Perm::#perm_conf{} | default) -> ok.
%% @doc Send messages to multiple clients. Default mean that there will be no
%% filtering. IE: All clients allowed to register to the channel will
%% receive the message.
%% @end
broadcast(Pid, Msgs, Perm) ->
    gen_server:cast(Pid, {broadcast, Msgs, Perm}).


-spec unicast(Pid::pid(),To::#client_state{},Msgs::[supercast_msg()]) -> ok.
%% @doc Send messages to an unique client.
unicast(Pid, To, Msgs) ->
    gen_server:cast(Pid, {unicast, To, Msgs}).


init([ChanName, InitialClient]) ->
    process_flag(trap_exit, true),
    case ets:lookup(?ETS_CHAN_STATES, ChanName) of
        [] ->
            %% channel has vanished
            {ok, "Channel has vanished"};
        _ ->
            %% Now the process can allready have in his queue a cast(delete)
            {ok, #state{chan_name=ChanName,clients=[InitialClient]}}
    end.

handle_call({subscribe2, _CState}, _From, State) ->
    %% @TODO get synchro info from ?ETS_CHAN_STATES#chan_state.module:join
    Pdus = [],
    {reply, {ok, Pdus}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(delete, #state{clients=_Clients,chan_name=Name} = S) ->
    %% @TODO send a unsubscribeOk message to all clients.
    {stop, {"Channel deleted", Name}, S};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{chan_name=Name}) ->
    supercast_reg:unregister_name(Name),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
