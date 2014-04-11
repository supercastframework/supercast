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
    behaviour_info/1,
    get_chan_perms/1,
    synchronize/2,
    subscribe/2,
    emit/2
]).

% BEHAVIOUR FOR DOCUMENTATION ONLY
-export([
    get_perms/1,
    sync_request/2
]).
% behaviour functions
behaviour_info(callbacks) ->
    [
        {get_perms,     1},
        {sync_request,  2}
    ];

behaviour_info(_) ->
    undefined.

% BEHAVIOUR FOR DOCUMENTATION ONLY
-spec get_perms(_PidName) -> _PermConf.
% @doc
% PidName = atom()
% PermConf = #perm_conf{}
% The module implementing supercast_channel behaviour must return the #perm_conf{}
% defining the authorisation to subscribe to him.
% Triggered on a client call to sync_request, supercast define if the user
% is allowed and continue or stop subscription process.
% @end
get_perms(_) ->
    #perm_conf{}.

-spec sync_request(_PidName, _ClientState) -> ok.
% @doc
% PidName = atom()
% ClientState = #client_state{}
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
sync_request(_,_) ->
    ok.

% API
-spec get_chan_perms(pid()) -> error | #perm_conf{}.
get_chan_perms(PidName) ->
    case module_from_pid(PidName) of
        error ->
            error;
        Mod ->
            Mod:get_perms(PidName)
    end.

-spec synchronize(pid(), #client_state{}) -> ok.
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
