%% @doc The wonderfull {{appid}} application.

-module({{appid}}).
-author("{{author}}").
-export([start/0, stop/0]).

%% supercast channel behaviour
-behaviour(supercast_channel).
-export([sync/3,leave/3]).

%% user test
-export([emit/2, send/2]).

%% @spec start() -> ok
%% @doc Start the {{appid}} server.
start() ->
    ok = application:start(xmerl),
    ok = application:start(supercast),
    ok = supercast:listen(),
    ChanName = "{{appid}}",
    Perm = undefined,
    Args = [],
    ok = supercast:new(ChanName, ?MODULE, Args, Perm).

stop() ->
    ChanName = "{{appid}}",
    supercast:delete(ChanName),
    init:stop().


%% @spec sync(ChanName::string(), Opts::any(), ClientState::any()) ->
%%      ok | {error, Reason::term()}
%% @doc
%% Wait for reply to the channel_worker process (wich will not emit data
%% because he is himself locking the channel.
%% @end
sync("{{appid}}", _Args, CState) ->
    Pdus = ["hello from jojo", "you should be synchro now"],
    supercast:sync_ack("{{appid}}", CState, Pdus);

sync(_, _, _) ->
    {error, "unknown channel"}.

%% @spec leave(Channel, Opts, CState) ->
%%      ok | {error, Reason::string()}
%% @doc
%% Return without waiting reply
%% @end
leave("{{appid}}", _Args, _CState) -> ok;
leave(_Channel, _Opts, _CState) -> {error, "unknown channel"}.


%% @spec send_broadcast(Messages::[term()], Perm::any()) -> ok
emit(Messages, undefined) ->
    Channel = "{{appid}}",
    supercast:emit(Channel, Messages);
emit(Messages, CustomPerm) ->
    Channel = "{{appid}}",
    supercast:emit(Channel, Messages, CustomPerm).

%% @spec send_unicast(Messages::[term()], CState) -> ok
send(Messages, CState) ->
    Channel = "{{appid}}",
    supercast:send(Channel, Messages, CState).


