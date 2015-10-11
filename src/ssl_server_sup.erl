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
-module(ssl_server_sup).
-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

start_link(Encoder, Port, MaxC, SslConf) ->
    Cert    = proplists:get_value(certificate, SslConf),
    CaCert  = proplists:get_value(caCertificate, SslConf),
    Key     = proplists:get_value(key, SslConf),
    Args    = [Encoder, Port, MaxC, Cert, CaCert, Key],
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([Encoder, Port, MaxC, Cert, CaCert, Key]) ->
    {ok,
        {
            {one_for_one, 1, 60},
            [
                {
                    ssl_listener,
                    {ssl_listener, start_link, [Port, ssl_client, MaxC]},
                    permanent,
                    2000,
                    worker,
                    [ssl_listener]
                },
                {
                    ssl_client_sup,
                    {ssl_client_sup, start_link, [Encoder, Key, Cert, CaCert]},
                    permanent,
                    infinity,
                    supervisor,
                    [ssl_client_sup]
                }
            ]
        }
    }.
