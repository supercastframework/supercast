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
% To implement a gen_channel a module must:
% <ul>
% <li>
%   also be a gen_server and understand these call/2
%   <ul>
%       <li>
%           call(Pid, get_perms), wich will return the #perm_conf{} 
%           restriction. This will tell supercast_mpd if the client is allowed to 
%           subscribe.
%       </li>
%       <li>
%           cast(Pid, {synchronize, CState}). When receiving this message, 
%           the module can eventualy send data to the client. After returning,
%           the module can send his periodic events and be sure the client is 
%           in sync.
%           Sending message should be done with supercast_mpd:unicast_msg/2 if the 
%           sender want supercast_mpd to filter message using the user_roles.
%       </li>
%   </ul>
% </li>
% <li>
%       Send every events of interest for clients in with the following funs:
%       <ul>
%           <li>
%               supercast_mpd:unicast_msg/2
%           </li>
%           <li>
%               supercast_mpd:multicast_msg/3
%           </li>
%       </ul>
% </li>
% </ul>
% @end
-module(gen_channel).
-export([
    behaviour_info/1,
    call/2,
    call/3,
    cast/2,
    cast/3
]).

behaviour_info(callbacks) ->
    [];

behaviour_info(_) ->
    undefined.

call(Channel, Msg) ->
    gen_server:call(Channel, Msg).

call(Channel, Msg, Timeout) ->
    gen_server:call(Channel, Msg, Timeout).

cast(Channel, Msg) ->
    gen_server:cast(Channel, Msg).

cast(Channel, Msg, Timeout) ->
    gen_server:cast(Channel, Msg, Timeout).
