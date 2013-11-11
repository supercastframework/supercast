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
-module(tcp_server_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

-define(MAXCONN, 50).

start_link(Port, Encoder, MaxConn) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port,Encoder,MaxConn]).

init([Port, Encoder, MaxC]) ->
	{ok,
		{
			{one_for_one, 1, 60},
			[
				{
					tcp_listener,
					{tcp_listener, start_link, 
                        [Port, tcp_client, MaxC]},
					permanent,
					2000,
					worker,
					[tcp_listener]
				},
				{
					tcp_client_sup,
					{tcp_client_sup, start_link, [Encoder]},
					permanent,
					infinity,
					supervisor,
					[tcp_client_sup]
				}
			]
		}
	}.
