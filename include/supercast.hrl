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
-define(LOGS(X), io:format("{~w, ~w}: DEBUG: ~p~n", [?MODULE, ?LINE, X])).

-record(perm_conf, {
    read    = []    :: [term()],
    write   = []    :: [term()]
}).

-record(chan, {
    id          = undefined :: atom(),
    perm        = undefined :: undefined | #perm_conf{}
}).

-record(client_state,  {
    socket,                     % client socket
    addr,                       % client address
    port,                       % client port
    certificate,                % ssl certificate
    ca_certificate,             % for self signed certs
    key,                        % ssl key
    ref,                        % reference ovoiding socket swap in the 
                                % middle of a async call
    user_name = [],             % user attached to the socket
    user_roles = [],            % groups wich the user belong
    user_modules,               % modules allowed at client connexion
    auth_request_count = 1,     % used by max request count
    module,                     % callback mod to send data
    encoding_mod,               %
    state,                      %
    pid                         % pid() of the gen_server howner of the socket
}). 

-record(supercast_module, {
    name        = undefined :: atom(),
    callback    = undefined :: module(),
    asnkey      = undefined :: atom(),
    static_chan = undefined :: pid() | reference(),
    perm        = []        :: [string()]
}).
