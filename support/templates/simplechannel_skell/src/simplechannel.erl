%% @doc The wonderfull {{appid}} application.

-module({{appid}}).
-include_lib("supercast/include/supercast.hrl").
-behaviour(supercast_proc).
-author("{{author}}").

-export([start/0, stop/0]).

%% supercast_proc  behaviour
-export([join_request/4,leave_request/4]).

%% user test
-export([emit/0, emit/2, send/2]).

start() ->
    ok = application:start(xmerl),
    ok = application:start(supercast),
    ok = supercast:listen(),
    ChanName = "{{appid}}",
    Perm = #perm_conf{read=["admin"], write=["admin"]},
    Args = [],
    ok = supercast_proc:new_channel(ChanName, ?MODULE, Args, Perm).

stop() ->
    ChanName = "{{appid}}",
    supercast_proc:delete_channel(ChanName),
    init:stop().

join_request("{{appid}}", _Args, _CState, Ref) ->
    Pdus = [
        [{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"you should be synced now">>}]
        ],
    supercast_proc:join_accept(Ref, Pdus);
    %supercast_channel:join_refuse(Ref);

join_request(_, _, _, _) ->
    {error, "unknown channel"}.

leave_request("{{appid}}", _Args, _CState, Ref) ->
    Pdus = [
        [{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"Bye!!!">>}]
    ],
    supercast_proc:leave_ack(Ref, Pdus).


emit() ->
    Event = [{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"Hello event">>}],
    Channel = "{{appid}}",
    supercast_proc:send_broadcast(Channel, [Event]).
emit(Messages, CustomPerm) ->
    Channel = "{{appid}}",
    supercast_proc:send_multicast(Channel, Messages, CustomPerm).

send(Messages, CState) ->
    Channel = "{{appid}}",
    supercast_proc:send_unicast(Channel, Messages, CState).
