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
%% @doc
%% A module using this behaviour can be used by <em>supercast</em> to
%% authenticate clients. The return will be used by the beha_supercast_acctrl
%% module used by the application
%% 
%% == authenticate/2 ==
%%
%% The only function to be exported is <em>authenticate/2</em>.
%%
%% <code>
%% authenticate(Uname, UPass) -> Any::term() | fail
%% 
%% <p>
%% Note that the returned term() can be anything but must be understandable 
%% by the beha_supercast_acctrl module used by the application.
%% </p>
%% </code>
%% @end
-module(supercast_auth).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{authenticate, 2}];

behaviour_info(_) ->
    undefined.
