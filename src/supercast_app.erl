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
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {file, "priv/index.html"}},
			{"/websocket", websocket_endpoint, []},
			{"/static/[...]", cowboy_static, {dir, "priv/static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(
		http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]).
