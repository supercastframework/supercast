% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Based on the work from Serge Aleynikov <saleyn at gmail.com> on the article
% www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
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
-module(ssl_client_sup).
-behaviour(supervisor).

-export([start_link/4, start_client/0]).
-export([init/1]).

start_link(Encoder, Key, Cert, CaCert) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, 
        [Encoder, Key, Cert, CaCert]).

%%-------------------------------------------------------------------------
%% @spec start_client() -> {ok, Pid}
%% @doc  Call from the listener to open a new client
%% @end
%%-------------------------------------------------------------------------
start_client() ->
    supervisor:start_child(?MODULE, []).

init([Encoder, Key, Cert, CaCert]) ->
    SslFiles = {Key, Cert, CaCert},
    {ok, {
        {simple_one_for_one, 10, 60},
            [
                {ssl_client,
                    {ssl_client, start_link, [Encoder, SslFiles]},
                    temporary,
                    brutal_kill,
                    worker,
                    [ssl_client]
                }
            ]
        }
    }.
