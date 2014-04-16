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
-module(supercast_clients_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(TcpClientConf, SslClientConf) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, 
            [TcpClientConf, SslClientConf]).

init([TcpClientConf, SslClientConf]) ->
    ModList1 = create_ssl_client(SslClientConf, []),
    ModList2 = create_tcp_client(TcpClientConf, ModList1),

    {ok,
        {
            {one_for_one, 10, 60},
            ModList2
        }
    }.

create_ssl_client(SslClientConf, List) ->
    case lists:keysearch(enabled, 1, SslClientConf) of
        {value, {enabled, true}} ->
            {value, {port,      Port}} = 
                lists:keysearch(port, 1, SslClientConf),
            {value, {ssl_conf,  SslConfFile}} =
                lists:keysearch(ssl_conf, 1, SslClientConf),
            {value, {encoder,   Encoder}} = 
                lists:keysearch(encoder, 1, SslClientConf),
            {value, {maxconn,   MaxConn}} = 
                lists:keysearch(maxconn, 1, SslClientConf),
            SupEntry = {
                ssl_server_sup,
                {ssl_server_sup, start_link, 
                    [Encoder, Port, MaxConn, SslConfFile]},
                permanent,
                2000,
                supervisor,
                [ssl_server_sup]
            },
            [SupEntry|List];
        _Other ->
            List
    end.

create_tcp_client(TcpClientConf,List) ->
    case lists:keysearch(enabled, 1, TcpClientConf) of
        {value, {enabled, true}} ->
            {value, {port,      Port}} = 
                lists:keysearch(port, 1, TcpClientConf),
            {value, {encoder,   Encoder}} = 
                lists:keysearch(encoder, 1, TcpClientConf),
            {value, {maxconn,   MaxConn}} = 
                lists:keysearch(maxconn, 1, TcpClientConf),
            SupEntry = {
                tcp_server_sup,
                {tcp_server_sup, start_link, [Port, Encoder, MaxConn]},
                permanent,
                2000,
                supervisor,
                [tcp_server_sup]
            },
            [SupEntry|List];
        _Other ->
            List
    end.
