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
-module(supercast_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, AuthModule}    = application:get_env(supercast, auth_module),
    {ok, AcctrlMod}     = application:get_env(supercast, acctrl_module),
    {ok, TcpClientConf} = application:get_env(supercast, tcp_client),
    {ok, SslClientConf} = application:get_env(supercast, ssl_client),
    {ok, PduDispatch}   = application:get_env(supercast, pdu_dispatch),
    {ok, MainChannels}  = application:get_env(supercast, main_channels),
    supercast_sup:start_link(
        {AuthModule, PduDispatch},              % for supercast_server
        {AcctrlMod, MainChannels},              % for supercast_mpd
        TcpClientConf,                          % for tcp_client_sup
        SslClientConf).                         % for ssl_client_sup

stop(_State) ->
	ok.
