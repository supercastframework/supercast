%% -----------------------------------------------------------------------------
%% Supercast Copyright (c) 2012-2016
%% Sebastien Serre <ssbx@sysmo.io> All Rights Reserved.
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
%% -----------------------------------------------------------------------------

%% @doc Simple authentication module reading "etc/users.txt" file.
%% Format must be of form
%% USER PASSWORD GROUP1,GROUP2
%% ...
%%
%% Every component is separed by one space only.
-module(supercast_auth_txt).
-behaviour(supercast_auth).

-export([authenticate/2]).

-define(USERS_XML, "etc/users.txt").

-spec authenticate(UName :: string(), UPass :: string()) ->
        Reply :: fail | {ok, Groups :: [string()]}.
authenticate(UName, UPass) ->
    {ok, IoDevice} = file:open(?USERS_XML, [read]),
    Rep = consume_file(UName, UPass, IoDevice),
    file:close(IoDevice),
    Rep.

consume_file(UName, UPass, IoDevice) ->
    case file:read_line(IoDevice) of
        eof -> fail;
        {error, _Reason} -> fail;
        {ok, String} ->
            String2 = string:strip(String, both),
            [UN, UP, GRPS] = string:token(String2, " "),
            case string:equal(UName, UN) of
                true ->
                    case string:equal(UPass, UP) of
                        true ->
                            {ok, GRPS};
                        false ->
                            consume_file(UName, UPass, IoDevice)
                    end;
                false ->
                    consume_file(UName, UPass, IoDevice)
            end
    end.



