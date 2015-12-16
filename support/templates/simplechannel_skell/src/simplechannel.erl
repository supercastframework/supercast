%% @doc The wonderfull {{appid}} application.

-module({{appid}}).
-include_lib("supercast/include/supercast.hrl").
-behaviour(supercast_channel).
-author("{{author}}").

-export([start/0, stop/0]).

%% supercast_channel  behaviour
-export([
    channel_init/2,
    channel_info/3,
    channel_join/3,
    channel_leave/3,
    channel_terminate/3]).

%% user test
-export([emit/0, emit/2, send/2]).

start() ->
    ok = application:start(xmerl),
    ok = application:start(supercast),
    ok = supercast:listen(),
    Perm = #perm_conf{read=["admin"], write=["admin"]},
    Args = [],
    supercast_channel:new("{{appid}}", ?MODULE, Args, Perm).

stop() ->
    supercast:info("{{appid}}", quit),
    supercast:info("{{appid}}", quit),
    init:stop().

channel_init("{{appid}}", _) ->
    {ok, nostate}.

channel_join("{{appid}}", _CState, State) ->
    Pdus = [[{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"you should be synced now">>}]],
    {ok, Pdus, State}.

channel_leave("{{appid}}", _CState, State) ->
    Pdus = [[{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"Bye!!!">>}]],
    {ok, Pdus, State}.

channel_info("{{appid}}", quit, State) ->
    {stop, normal, State}.

channel_terminate("{{appid}}", _Reason, _State) ->
    Pdus = [[{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"Aouch!">>}]],
    {ok, Pdus}.

emit() ->
    Event = [{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"Hello event">>}],
    supercast_proc:send_broadcast("{{appid}}", [Event]).

emit(Messages, CustomPerm) ->
    supercast_proc:send_multicast("{{appid}}", Messages, CustomPerm).

send(Messages, CState) ->
    supercast_proc:send_unicast("{{appid}}", Messages, CState).
