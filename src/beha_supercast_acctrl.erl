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
% @doc
% A module implementing this behaviour must export the function statisfy/3.
% statisfy/2 must take as arguments read | write a list of #client_state 
% and a term().
% Return a list of #client_state wich satisfy with the access control
% constraint defined by term().
% term() is dependant on the return of beha_supercast_auth module wich is used by 
% the application.
% @end
-module(beha_supercast_acctrl).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {satisfy, 3}
    ];

behaviour_info(_) ->
    undefined.
