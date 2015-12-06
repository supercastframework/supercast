%% -------------------------------------------------------------------
%% Supercast Copyright (c) 2012-2015 Sebastien Serre <ssbx@sysmo.io>
%% All Rights Reserved.
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

%% @private
-module(supercast_registrar).
-behaviour(gen_server).
-include("supercast.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/0]).

-export([register_name/2, unregister_name/1, whereis_name/1,
    send/2, which_module/1]).

register_name({Module, Name}, Pid) ->
    gen_server:call(?MODULE, {register_name, Name, Module, Pid}).

unregister_name(Name) ->
    gen_server:call(?MODULE, {unregister_name, Name}).

whereis_name(Name) ->
    gen_server:call(?MODULE, {whereis_name, Name}).

which_module(Name) ->
    gen_server:call(?MODULE, {which_module, Name}).

send(Name,Msg) ->
    case whereis_name(Name) of
        Pid when is_pid(Pid) ->
            Pid ! Msg,
            Pid;
        undefined ->
            exit({badarg, {Name,Msg}})
    end.

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
%% @private
init([]) ->
    {ok, []}.

%% @private
handle_call({register_name, Name, Module, Pid}, _F, R) ->
    case lists:keymember(Name,2,R) of
        true ->
            {reply, no, R};
        false ->
            case lists:keymember(Pid,3,R) of
                true ->
                    {reply, no, R};
                false ->
                    NewEntry = #registered_chan{
                        name    = Name,
                        pid     = Pid,
                        module  = Module
                    },
                    {reply, yes, [NewEntry|R]}
        end
    end;

handle_call({unregister_name, Name}, _F, R) ->
    case lists:keytake(Name,2,R) of
        false ->
            {reply, Name, R};
        {value,_,NewR} ->
            {reply, Name, NewR}
    end;

handle_call({whereis_name, Name}, _F, R) ->
    case lists:keyfind(Name,2,R) of
        false ->
            {reply, undefined, R};
        #registered_chan{pid=Pid} ->
            {reply, Pid, R}
    end;

handle_call({which_module, Name}, _F, R) ->
    case lists:keyfind(Name, 2, R) of
        false ->
            {reply, error, R};
        #registered_chan{module=Mod} ->
            {reply, Mod, R}
    end;

handle_call(_Call, _F, S) ->
    ?SUPERCAST_LOG_WARNING("Unknown call", _Call),
    {noreply, S}.

%% CAST
%% @private
handle_cast(_Cast, S) ->
    ?SUPERCAST_LOG_WARNING("Unknown cast", _Cast),
    {noreply, S}.

%% OTHER
%% @private
handle_info(_I, S) ->
    {noreply, S}.

%% @private
terminate(_R, _S) ->
    normal.

%% @private
code_change(_O, S, _E) ->
    {ok, S}.
