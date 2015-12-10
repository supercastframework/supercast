%% @doc The wonderfull {{appid}} application.

-module({{appid}}).
-author("{{author}}").
-export([start/0, stop/0]).

%% supercast channel behaviour
-export([synchronize/3,disconnect/3]).

%% user test
-export([broadcast/2, unicast/2]).

%% @spec start() -> ok
%% @doc Start the {{appid}} server.
start() ->
    ok = application:start(xmerl),
    ok = application:start(supercast),
    ok = supercast:listen(),
    ChanName = "{{appid}}",
    Perm = undefined,
    Opts = [],
    ok = supercast:create(ChanName, ?MODULE, Opts, Perm).

stop() ->
    ChanName = "{{appid}}",
    supercast:delete(ChanName),
    init:stop().


%% @spec synchronize(ChanName::string(), Opts::any(), ClientState::any()) ->
%%      {ok, Pdus::[term()]} | {error, Reason::term()}
%% @doc
%% Wait for reply to the channel_worker process (wich will not emit data
%% because he is himself locking the channel.
%% @end
synchronize("{{appid}}", _Opts, _CState) ->
    {ok, ["hello from jojo", "you should be synchro now"]};
synchronize(_, _, _) ->
    {error, "unknown channel"}.

%% @spec leave(Channel, Opts, CState) ->
%%      ok | {error, Reason::string()}
%% @doc
%% Return without waiting reply
%% @end
disconnect("{{appid}}", _Opts, _CState) -> ok;
disconnect(_Channel, _Opts, _CState) -> {error, "unknown channel"}.


%% @spec send_broadcast(Messages::[term()], Perm::any()) -> ok
broadcast(Messages, undefined) ->
    Channel = "{{appid}}",
    supercast:broadcast(Channel, Messages);
broadcast(Messages, CustomPerm) ->
    Channel = "{{appid}}",
    supercast:broadcast(Channel, Messages, CustomPerm).

%% @spec send_unicast(Messages::[term()], CState) -> ok
unicast(Messages, CState) ->
    Channel = "{{appid}}",
    supercast:unicast(Channel, Messages, CState).


