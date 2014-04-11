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
-include("include/supercast.hrl").
-export([
    get_chan_perms/1,
    synchronize/2,
    subscribe/2,
    emit/2
]).

-callback get_perms(PidName::atom()) -> #perm_conf{}.
% @doc
% The module implementing supercast_channel behaviour must return the #perm_conf{}
% defining the authorisation to subscribe to him.
% Triggered on a client call to sync_request, supercast define if the user
% is allowed and continue or stop subscription process.
% @end

-callback sync_request(PidName::atom(), CState::#client_state{}) -> ok.
% @doc
% This call is triggered when a client allowed to subscribe to the channel.
% - The sync_request must include a supercast_channel:subscribe/2 if the channel want
% to forward future messages to the client. 
% - It must also dump the channel state using the 
% ClientState#client_state.module:send/2
% Note that these two actions must be done in a single
% call (gen_server:cast, gen_fsm:send_event) for a perfect synchronisation.
% It is better to use a asynchronous call to not block the supercast_mpd during
% the dump.
% @end

% API
-spec get_chan_perms(pid()) -> error | any().
get_chan_perms(PidName) ->
    case module_from_pid(PidName) of
        error ->
            error;
        Mod ->
            Mod:get_perms(PidName)
    end.

-spec synchronize(pid(), #client_state{}) -> error | ok.
synchronize(PidName, CState) ->
    case module_from_pid(PidName) of
        error ->
            error;
        Mod ->
            Mod:sync_request(PidName, CState),
            ok
    end.

-spec subscribe(atom(), #client_state{}) -> ok.
subscribe(PidName, CState) ->
    supercast_mpd:subscribe_stage3(PidName, CState).

-spec emit(atom(), {#perm_conf{}, tuple()}) -> ok.
emit(PName, {Perms, Pdu}) ->
    supercast_mpd:multicast_msg(PName, {Perms, Pdu}).


-spec module_from_pid(PidName::atom()) -> error | atom().
% @private
module_from_pid(PidName) ->
    case whereis(PidName) of
        undefined ->
            error;
        Pid ->
            ProcInfo    = erlang:process_info(Pid, [dictionary]),
            case ProcInfo of
                [{dictionary, L}] ->
                    {'$initial_call', {Mod,_,_}} = lists:keyfind('$initial_call',1,L),
                    Mod;
                _ ->
                    error
            end
    end.
