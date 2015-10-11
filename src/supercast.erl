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
-module(supercast).
-include("include/supercast.hrl").

-export([
    filter/2,
    satisfy/2,
    mpd_state/0,
    server_state/0
]).

satisfy(CState, Perm) ->
    {ok, AccCtrl} = application:get_env(supercast, acctrl_module),
    case AccCtrl:satisfy(read, [CState], Perm) of
        {ok, []}        -> false;
        {ok, [CState]}  -> true
    end.

-spec filter(#client_state{}, [{#perm_conf{}, any()}]) -> [any()].
% @doc
% Called by external modules to filter things. It will return any "things",
% that the client defined in #client_state{} is allowed to 'read'.
% @end
filter(CState, Things) ->
    filter_things(CState, Things, []).

filter_things(_, [], R) ->
    R;
filter_things(CState, [{Perm, Thing}|T], R) ->
    case satisfy([CState], Perm) of
        false ->
            filter_things(CState, T, R);
        true  ->
            filter_things(CState, T, [Thing|R])
    end.

-spec mpd_state() -> {ok, tuple()}.
% @doc
% This function return the state of the supercast_mpd gen_server module.
% @end
mpd_state() ->
    gen_server:call(supercast_mpd, dump).

-spec server_state() -> {ok, tuple()}.
% @doc
% This function return the state of the supercast_server gen_server module.
% @end
server_state() ->
    gen_server:call(supercast_server, dump).
