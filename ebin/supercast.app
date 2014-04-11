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
{application, supercast,
    [
        {description, "Supercast, InterFace Session/authentication"},
        {vsn, "2.0.0"},
        {modules, [
                supercast_app,
                supercast_sup,
                supercast_mpd,
                supercast_server,
                supercast,
                clsupercast,
                ssl_client,
                ssl_client_sup,
                ssl_server_sup,
                ssl_listener,
                tcp_client,
                tcp_client_sup,
                tcp_server_sup,
                tcp_listener,

                supercast_channel,
                supercast_acctrl,
                supercast_auth,
                supercast_encoder,

                supercast_acctrl_rbac,
                supercast_auth_ldap,
                supercast_auth_local,
                supercast_encoder_asn,
                supercast_encoder_json,
                supercast_encoder_native,
                'NmsPDU',
                'ModSupercast',
                'ModTracker'
            ]},
        {registered, [
                supercast_sup,
                supercast_server,
                supercast_mpd,
                ssl_client_sup,
                ssl_server_sup,
                ssl_listener,
                tcp_server_sup,
                tcp_client_sup,
                tcp_listener
            ]},
        {applications, 
            [kernel, stdlib, crypto, public_key, ssl]
        },
        {mod, {supercast_app, []}}
    ]
}.
