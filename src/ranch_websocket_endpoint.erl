-module(ranch_websocket_endpoint).
-behaviour(cowboy_websocket_handler).
-include("supercast.hrl").
-include("logs.hrl").

-export([init/3]).
% cowboy_ewebsocket_handler
-export([websocket_init/3,websocket_handle/3,
    websocket_info/3,websocket_terminate/3]).
% supercast
-export([auth_set/2,auth_set/5,send/2,raw_send/2]).

%% @spec auth_set(success, #client_state{}, Name, Roles, AllowedMods) -> ok
%% @doc
%% Set the client authentication tokens
%% @end
auth_set(auth_success, #client_state{pid=Pid, ref=Ref},
        Name, Roles, AllowedMods) ->
    cast(Pid, {auth_success, Ref, Name, Roles, AllowedMods}).

%% @spec auth_set(success, #client_state{}) -> ok
%% @doc
%% Inform client of authentication failure
%% @end
auth_set(auth_fail, #client_state{pid=Pid, ref=Ref, user_name=UserName}) ->
    cast(Pid, {auth_fail, Ref, UserName}).

%% @spec send(#client_state{}, {pdu, Message}) -> ok
%% @doc
%% Send a message to the client
%% @end
send(#client_state{pid=Pid, ref=Ref}, Message) ->
    cast(Pid, {encode_send, Ref, Message}).

%% @spec send(#client_state{}, {pdu, Message}) -> ok
%% @doc
%% Send a pdu to the client
%% @end
raw_send(#client_state{pid=Pid, ref=Ref}, Pdu) ->
    cast(Pid, {send, Ref, Pdu}).

init({tcp, http}, _Req, _Opts) ->
    ?LOG_INFO("init http"),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    ?LOG_INFO("init websocket"),
	erlang:start_timer(1000, self(), <<"Hello!">>),
    State = #client_state{pid=self(), ref=make_ref(),
        module=?MODULE, authenticated=false},
	{ok, Req, State}.

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    ?LOG_INFO("Unknown handle", {_Data,Req,State}),
	{ok, Req, State}.

websocket_info({send, Ref, _Pdu}, Req, #client_state{ref=Ref} = State) ->
    {ok, Req, State};
websocket_info({encode_send, Ref, _Msg},
        Req, #client_state{ref=Ref} = State) ->
    {ok, Req, State};
websocket_info({auth_success, _Ref, _Name, _Roles, _AllowedMods},
        Req, #client_state{ref=_Ref} = State) ->
    {ok, Req, State};
websocket_info({auth_fail, _Ref, _UserName},
        Req, #client_state{ref=_Ref} = State) ->
    {ok, Req, State};
websocket_info({timeout, _Ref, Msg}, Req, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    ?LOG_INFO("Unknown info", {_Info,Req,State}),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
    supercast_server:client_msg(disconnect, State),
    ?LOG_INFO("Terminated", {_Reason,_Req,State}),
	ok.

cast(Pid, Val) -> Pid ! Val, ok.
