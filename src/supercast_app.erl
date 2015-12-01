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
    Ret = supercast_sup:start_link(),
    start_listening(),
    Ret.

stop(_State) ->
    ok.

start_listening() ->
    {ok, DocRoot} = application:get_env(supercast, http_docroot),
    DocrootPath = filename:absname(DocRoot),
    DocrootIndex = filename:join(DocrootPath, "index.html"),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", ranch_websocket_endpoint, []},
            {"/", cowboy_static, {file, DocrootIndex, [{etag,false}]}},
            {"/[...]", cowboy_static, {dir, DocrootPath, [{etag,false}]}}
        ]}
    ]),

    {ok, HTTPPort} = application:get_env(supercast, http_port),
    %% TODO compress does not work
    %% TODO keep-alive does not work
    {ok, _} = cowboy:start_http(supercast_http, 50, [{port, HTTPPort}], [
        {compress, true},
        {env, [{dispatch, Dispatch}]},
        {max_keepalive, 50}
    ]),

    {ok, TCPPort} = application:get_env(supercast, tcp_port),
    {ok, _} = ranch:start_listener(supercast_tcp, 10, ranch_tcp,
        [{port, TCPPort}], ranch_tcp_endpoint, []).
