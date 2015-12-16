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
-export([emit/0,close/0]).

start() ->
    ok = application:start(xmerl),
    ok = application:start(supercast),
    ok = supercast:listen(),
    Perm = #perm_conf{read=["admin"], write=["admin"]},
    Args = [],
    supercast_channel:new("{{appid}}", ?MODULE, Args, Perm).

stop() ->
    init:stop().

channel_init("{{appid}}", _Args) ->
    {ok, nostate}.

channel_join("{{appid}}", _CState, State) ->
    Pdus = [[{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"you should be synced now">>}]],
    {ok, Pdus, State}.

channel_leave("{{appid}}", _CState, State) ->
    Pdus = [[{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"Bye!!!">>}]],
    {ok, Pdus, State}.

channel_info("{{appid}}", {emit, Events}, State) ->
    supercast:emit("{{appid}}", Events),
    {ok, State};
channel_info("{{appid}}", {emit, Events, Perm}, State) ->
    supercast:emit("{{appid}}", Events, Perm),
    {ok, State};
channel_info("{{appid}}", quit, State) ->
    {stop, normal, State}.

channel_terminate("{{appid}}", _Reason, _State) ->
    Pdus = [[{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"Aouch!">>}]],
    {ok, Pdus}.

emit() ->
    Event = [{<<"from">>, <<"{{appid}}">>}, {<<"value">>, <<"Hello event">>}],
    supercast:info_request("{{appid}}", {emit, [Event]}).

close() ->
    supercast:info_request("{{appid}}", quit).
