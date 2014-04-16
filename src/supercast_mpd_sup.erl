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
-module(supercast_mpd_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

start_link(MpdConf, TcpClientConf, SslClientConf) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, 
            [MpdConf, TcpClientConf, SslClientConf]).

init([MpdConf, TcpClientConf, SslClientConf]) ->
    SupercastMpd = {
        supercast_mpd,
        {supercast_mpd,start_link, [MpdConf]},
        permanent,
        2000,
        worker,
        [supercast_mpd]
    },
    ClientsSup = {
        supercast_clients_sup,
        {supercast_clients_sup, start_link, [TcpClientConf, SslClientConf]},
        permanent,
        infinity,
        supervisor,
        [supercast_clients_sup]
    },

    {ok,
        {
            {rest_for_one, 1, 300},
            [SupercastMpd, ClientsSup]
        }
    }.
