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

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, SslConf} = application:get_env(supercast, ssl_client),
    {ok, TcpConf} = application:get_env(supercast, tcp_client),

    case proplists:get_value(enabled, SslConf, false) of
        false ->
            ML = [];
        true ->
            ML = [create_ssl_client(SslConf)]
    end,
    case proplists:get_value(enabled, TcpConf, false) of
        false ->
            ML2 = ML;
        true ->
            ML2 = [create_tcp_client(TcpConf)|ML]
    end,

    {ok,
        {
            {one_for_one, 10, 60},
            ML2
        }
    }.

create_ssl_client(Cfg) ->
    Port = proplists:get_value(port, Cfg),
    SslC = proplists:get_value(ssl_conf, Cfg),
    Enc  = proplists:get_value(encoder, Cfg),
    MaxC = proplists:get_value(maxconn, Cfg),

    {
        ssl_server_sup,
        {ssl_server_sup, start_link, [Enc, Port, MaxC, SslC]},
        permanent,
        2000,
        supervisor,
        [ssl_server_sup]
    }.

create_tcp_client(Cfg) ->
    Port = proplists:get_value(port, Cfg),
    Enc  = proplists:get_value(encoder, Cfg),
    MaxC = proplists:get_value(maxconn, Cfg),

    {
        tcp_server_sup,
        {tcp_server_sup, start_link, [Port, Enc, MaxC]},
        permanent,
        2000,
        supervisor,
        [tcp_server_sup]
    }.
