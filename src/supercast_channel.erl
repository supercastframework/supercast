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
% @doc
% @end
-module(supercast_channel).
-include("supercast.hrl").

% local API
-export([
    get_chan_perms/1,
    synchronize/2
]).

% users API
-export([
    subscribe/2,
    emit/2,
    unicast/2,
    delete/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% supercast_channel behaviour callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-callback get_perms(Channel::string()) -> #perm_conf{}.
% @doc
% The module implementing supercast_channel behaviour must return the #perm_conf{}
% defining the authorisation to subscribe to him.
% Triggered on a client call to sync_request, supercast define if the user
% is allowed and continue or stop subscription process.
% @end

-callback sync_request(Channel::string(), CState::#client_state{}) -> ok.
% @doc
% This call is triggered when a client allowed to subscribe to the channel.
% - The sync_request must include a supercast_channel:subscribe/2 if the channel want
% to forward future messages to the client. 
% - It can also dump the channel state using the 
% ClientState#client_state.module:send/2
% Note that these two actions must be done in a single
% call (gen_server:cast, gen_fsm:send_event) for a perfect synchronisation.
% You must use a asynchronous call to not block the supercast_mpd during
% the sync.
% @end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUPERCAST server and mpd API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(Channel) ->
    supercast_mpd:delete_channel(Channel).

-spec get_chan_perms(Channel::string()) -> error | #perm_conf{}.
% @private
get_chan_perms(Channel) ->
    case supercast_registrar:which_module(Channel) of
        error ->
            error;
        Mod ->
            Mod:get_perms(Channel)
    end.

-spec synchronize(Channel::string(), #client_state{}) -> error | ok.
% @private
synchronize(Channel, CState) ->
    case supercast_registrar:which_module(Channel) of
        error ->
            error;
        Mod ->
            Mod:sync_request(Channel, CState),
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% channels API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec subscribe(Channel::string(), CState::#client_state{}) -> ok.
subscribe(Channel, CState) ->
% @doc
% Called by a channel to subscribe a client to himself. Every following
% emit/2 messages will then be delivered to the client.
% This function is typicaly called in the sync_request/2 callback. 
% @end
    supercast_mpd:subscribe_stage3(Channel, CState).

-spec emit(Channel::string(), {PermConf::#perm_conf{}, Pdu::tuple()}) -> ok.
% @doc
% Used by a channel to send a message to all sbscribers.
% @end
emit(Channel, {Perms, Pdu}) ->
    supercast_mpd:multicast_msg(Channel, {Perms, Pdu}).

-spec unicast(CState::#client_state{}, Msgs::[supercast_msg()]) -> ok.
% @doc
% supercast_msg()   :: {function, fun()} | {pdu, tuple()}.
%
% Used by a channel to send a list of message or funs to a single client
% identified by CState wich is a #client_state.
% fun() will just be executed as is in the client loop with #client_state{}
% as argument. You will need tu use it (specialy #client_state.socket and
% #client_state.encoding_mod) to encode and send messages.
% You must use the client_state to send messages when building the fun.
% @end
unicast(_, []) ->
    ok;
unicast(CState, [Elem|Elems]) ->
    Mod = CState#client_state.module,
    Mod:send(CState, Elem),
    unicast(CState, Elems).
