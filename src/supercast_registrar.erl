%% -------------------------------------------------------------------
%% Supercast Copyright (c) 2012-2015
%% Sebastien Serre <ssbx@supercastframework.org> All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%% -------------------------------------------------------------------

%% @doc Keep a state of channel pids.
%% Channel names are dynamic. We can not store these values with atoms
%% because they are not garbage collected and limited. This module implement
%% the required functions to implement a local pid registry using strings as
%% pid name.
%% @end

-module(supercast_registrar).
-include("supercast.hrl").

-export([register_name/2,unregister_name/1,whereis_name/1,send/2]).

-spec register_name(Name::string(), Pid::pid()) -> yes | no.
register_name(Name, Pid) ->
    case where(Name) of
        undefined ->
            true = ets:insert(?ETS_RELAYS_REGISTER, {Name, Pid}),
            yes;
        _ ->
            no
    end.

-spec unregister_name(Name::string()) -> Name::string().
unregister_name(Name) ->
    true = ets:delete(?ETS_RELAYS_REGISTER, Name),
    Name.

-spec whereis_name(Name::string()) -> pid() | undefined.
whereis_name(Name) -> where(Name).
where(Name) ->
    case ets:lookup(?ETS_RELAYS_REGISTER, Name) of
        [{Name,Pid}] ->
            case is_process_alive(Pid) of
                true  -> Pid;
                false -> undefined
            end;
        [] -> undefined
    end.

-spec send(Name::string, Msg::term()) -> pid().
send(Name, Msg) ->
    case where(Name) of
        Pid when is_pid(Pid) ->
            Pid ! Msg,
            Pid;
        undefined ->
            exit({badarg, {Name, Msg}})
    end.


