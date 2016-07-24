%% @doc The wonderfull {{name}} application.

-module({{name}}).
-include_lib("supercast/include/supercast.hrl").
-behaviour(supercast_channel).
-author("{{author_name}}").

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
    supercast_channel:new("{{name}}", ?MODULE, Args, Perm).

stop() ->
    init:stop().

channel_init("{{name}}", _Args) ->
    {ok, nostate}.

channel_join("{{name}}", _CState, State) ->
    Pdus = [[{<<"from">>, <<"{{name}}">>}, {<<"value">>, <<"you should be synced now">>}]],
    {ok, Pdus, State}.

channel_leave("{{name}}", _CState, State) ->
    Pdus = [[{<<"from">>, <<"{{name}}">>}, {<<"value">>, <<"Bye!!!">>}]],
    {ok, Pdus, State}.

channel_info("{{name}}", {emit, Events}, State) ->
    supercast:emit("{{name}}", Events),
    {ok, State};
channel_info("{{name}}", {emit, Events, Perm}, State) ->
    supercast:emit("{{name}}", Events, Perm),
    {ok, State};
channel_info("{{name}}", quit, State) ->
    {stop, normal, State}.

channel_terminate("{{name}}", _Reason, _State) ->
    Pdus = [[{<<"from">>, <<"{{name}}">>}, {<<"value">>, <<"Aouch!">>}]],
    {ok, Pdus}.

emit() ->
    Event = [{<<"from">>, <<"{{name}}">>}, {<<"value">>, <<"Hello event">>}],
    supercast:info_request("{{name}}", {emit, [Event]}).

close() ->
    supercast:info_request("{{name}}", quit).
