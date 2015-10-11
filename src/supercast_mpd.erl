% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
%
% Enms is a Network Management System aimed to manage and monitor SNMP
% targets, monitor network hosts and services, provide a consistent
% documentation system and tools to help network professionals
% to have a wide perspective of the networks they manage.
%
% Enms is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% Enms is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with Enms.  If not, see <http://www.gnu.org/licenses/>.
% @private
-module(supercast_mpd).
-behaviour(gen_server).
-include("include/supercast.hrl").
-include_lib("common_hrl/include/logs.hrl").

% TODO should use ets for state

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/0,
    multicast_msg/2,
    unicast_msg/2,
    subscribe_stage1/2,
    subscribe_stage2/2,
    subscribe_stage3/2,
    unsubscribe/2,
    main_chans/0,
    client_disconnect/1,
    delete_channel/1
]).

-record(state, {
    acctrl,
    main_chans,
    chans
}).

-define(CHAN_TIMEOUT, 1000).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

delete_channel(Channel) ->
    gen_server:cast(?MODULE, {delete_channel, Channel}).

-spec main_chans() -> [atom()].
% @doc
% Called by supercast_server.
% Called at the initial connexion of a client. Give him the main (static)
% channels. If dynamic channels exist in the application, this is the role
% of one of these channels to inform the client. Note that depending on the
% supercast_channel module perm/0 function return, a client might not be able to
% subscribe to a channel apearing here. It is checked at the
% subscribe_stage1/1 call.
% @end
main_chans() ->
    gen_server:call(?MODULE, main_chans).

-spec subscribe_stage1(atom(), #client_state{}) -> ok | error.
% @doc
% Called by a client via supercast_server.
% If return is ok, the supercast_server will then call subscribe_stage2.
% If return is error, do nothing more.
% In both case, supercast_server interpret the return and send subscribeErr or
% subscribeOk to the client.
% @end
subscribe_stage1(Channel, CState) ->
    % Does the channel exist?
    Rep = supercast_channel:get_chan_perms(Channel),
    ?LOG_INFO("Subscribe statge 1", {Channel, Rep}),
    case Rep of
        #perm_conf{} = Perm ->
            gen_server:call(?MODULE, {subscribe_stage1, Channel, CState, Perm});
        _ ->
            error
    end.

-spec subscribe_stage2(atom(), #client_state{}) -> ok.
% @doc
% Called by supercast_server after a supercast_mpd:subscribe_stage1/2 success. The call
% will trigger a subscribe_stage3/2 called by the channel. sync sync.
% @end
subscribe_stage2(Channel, CState) ->
    case gen_server:call(?MODULE, {client_is_registered, Channel, CState}) of
        true ->
            %client allready registered do nothing
            ok;
        false ->
            try supercast_channel:synchronize(Channel, CState) of
                ok ->
                    ok
                catch
                    _:_ -> error
            end
    end.

-spec subscribe_stage3(atom(), #client_state{}) -> ok.
% @doc
% Called by a channel after a supercast_mpd:subscribe_stage2/2 success.
% In fact, the client will really be subscribed here by the channel.
% @end
subscribe_stage3(Channel, CState) ->
    gen_server:call(?MODULE, {subscribe_stage3, Channel, CState}).

-spec multicast_msg(atom(), {#perm_conf{}, tuple()}) -> ok.
% @doc
% Called by a supercast_channel module with a message that can be of interest for
% clients that have subscribed to the channel.
% Will be send depending of the right of the user.
% @end
multicast_msg(Chan, {Perm, Pdu}) ->
    gen_server:cast(?MODULE, {multicast, Chan, Perm, Pdu}).

-spec unicast_msg(#client_state{}, tuple()) -> ok.
% @doc
% Called by a supercast_channel module with a message for a single client. Message
% will or will not be sent to the client depending on the permissions.
% Note that the channel is not checked. Thus a client wich is not subscriber
% of any channels can receive these messages.
% Typicaly used when the supercast_channel need to synchronize the client using his
% handle_cast({synchronize, CState}, State) function.
% @end
unicast_msg(CState, {Perm, Pdu}) ->
    gen_server:cast(?MODULE, {unicast, CState, Perm, Pdu}).

-spec client_disconnect(#client_state{}) -> ok.
% @doc
% Called by supercast_server when the client disconnect.
% Result in removing the client state from all the initialized channels.
% @end
client_disconnect(CState) ->
    gen_server:call(?MODULE, {client_disconnect, CState}).

-spec unsubscribe(atom(), #client_state{}) -> ok.
% @doc
% Called by a client using the supercast asn API via supercast_server.
% @end
unsubscribe(Chan, CState) ->
    gen_server:call(?MODULE, {unsubscribe, Chan, CState}).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
init([]) ->
    {ok, AcctrlMod}    = application:get_env(supercast, acctrl_module),
    {ok, MainChannels} = application:get_env(supercast, main_channels),
    {ok, #state{
            acctrl     = AcctrlMod,
            main_chans = MainChannels,
            chans      = []
        }
    }.

handle_call(main_chans, _F, #state{main_chans = Chans} = S) ->
    {reply, Chans, S};

handle_call({client_is_registered, Chan, CState}, _F,
        #state{chans = Chans} = S) ->
    case lists:keyfind(Chan, 1, Chans) of
        {Chan, CList} ->
            {reply, lists:member(CState, CList), S};
        false ->
            {reply, false, S}
    end;

handle_call({subscribe_stage1, _Channel, CState, PermConf},  _F,
        #state{acctrl = Acctrl} = S) ->
    case Acctrl:satisfy(read, [CState], PermConf) of
        {ok, []} ->
            {reply, error, S};
        {ok, [CState]} ->
            {reply, ok, S}
    end;
% subscribe_stage2/2 do not use the server
handle_call({subscribe_stage3, Channel, CState}, _F, S) ->
    Chans = new_chan_subscriber(CState, Channel, S#state.chans),
    {reply, ok, S#state{chans = Chans}};


handle_call({unsubscribe, Channel, CState}, _F, S) ->
    Chans = del_chan_subscriber(CState, Channel, S#state.chans),
    {reply, ok, S#state{chans = Chans}};



handle_call({client_disconnect, CState}, _F, S) ->
    Chans = del_subscriber(CState, S#state.chans),
    {reply, ok, S#state{chans = Chans}};

handle_call(get_acctrl, _F, #state{acctrl = Acctrl} = S) ->
    {reply, {ok, Acctrl}, S};

handle_call(dump, _F, S) ->
    {reply, {ok, S}, S};

handle_call(Call, _F, S) ->
    ?LOG_ERROR("Handle unknown call", Call),
    {reply, error, S}.

% CAST
handle_cast({delete_channel, Channel}, #state{chans=Chans} = S) ->
    Channels = proplists:delete(Channel, Chans),
    {noreply, S#state{chans=Channels}};

% called by himself
handle_cast({unicast, #client_state{module = CMod} = CState, Perm, Pdu},
    #state{acctrl = AcctrlMod} = S) ->
    case AcctrlMod:satisfy(read, [CState], Perm) of
        {ok, []} ->
            {noreply, S};
        {ok, [CState]} ->
            CMod:send(CState, Pdu),
            {noreply, S}
    end;

handle_cast({multicast, Chan, Perm, Pdu},
        #state{chans = Chans, acctrl = AcctrlMod} = S) ->
    % the chan have allready been initialized?
    case lists:keyfind(Chan, 1, Chans) of
        % no do nothing
        false ->
            ok;
        % yes but is empty do nothing
        {Chan, []} ->
            ok;
        % yes then filter with satisfy:
        {Chan, CList} ->
            % take the list of clients wich satisfy Perm
            {ok, AllowedCL} = AcctrlMod:satisfy(read, CList, Perm),
            % encode/send
            send(AllowedCL, Pdu)
    end,
    {noreply, S};

handle_cast(Cast, S) ->
    ?LOG_ERROR("Unknown cast", Cast),
    {noreply, S}.

% OTHER
handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.

% filter each encoding types
send(AllowedCL, Pdu) ->
    send(AllowedCL, Pdu, []).
send([], Pdu, Processed) ->
    send_type(Pdu, Processed);
send([Client|Rest], Pdu, Processed) ->
    #client_state{encoding_mod = Encode} = Client,
    case lists:keyfind(Encode, 1, Processed) of
        false ->
            NewP = lists:keystore(Encode, 1, Processed, {Encode, [Client]}),
            send(Rest, Pdu, NewP);
        {Encode, Clients} ->
            NewP = lists:keystore(Encode, 1, Processed, {Encode, [Client|Clients]}),
            send(Rest, Pdu, NewP)
    end.

% encode once
send_type(_, []) -> ok;
send_type(Pdu, [CType|Clients]) ->
    {Encode, ClientsOfType} = CType,
    Pdu2 = Encode:encode(Pdu),
    send_clients(Pdu2, ClientsOfType),
    send_type(Pdu, Clients).

% send multiple
send_clients(_, []) -> ok;
send_clients(Pdu, [Client|Others]) ->
    #client_state{module = Mod} = Client,
    Mod:raw_send(Client, Pdu),
    send_clients(Pdu, Others).





% PRIVATE
new_chan_subscriber(CState, Channel, Chans) ->
    % does the chan allready exist?
    case lists:keyfind(Channel, 1, Chans) of
        false ->
            % then create it
            NewChan = {Channel, [CState]},
            % return the new chan list
            [NewChan |Chans];
        {Channel, CList} ->
            % then add the new client
            NewChan = {Channel, [CState | CList]},
            % return a new Chans list with updated tuple
            lists:keyreplace(Channel, 1, Chans, NewChan)
    end.

del_chan_subscriber(CState, Channel, Chans) ->
    case lists:keyfind(Channel, 1, Chans) of
        false -> Chans;
        {Channel, CList} ->
            case lists:delete(CState, CList) of
                [] ->
                    % if list empty, delete the channel tuple
                    ?LOG_INFO("Channel deleted", Channel),
                    lists:keydelete(Channel, 1, Chans);
                NewCList ->
                    lists:keyreplace(Channel, 1, Chans, {Channel, NewCList})
            end
    end.

del_subscriber(CState, Chans) ->
    lists:foldl(fun({Id, CList}, Acc) ->
        N = lists:delete(CState, CList),
        [{Id, N} | Acc]
    end, [], Chans).
