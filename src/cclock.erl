%%
%% Copyright 2013 Joaquim Rocha
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(cclock).

-behaviour(gen_server).

-define(SERVER, {local, ?MODULE}).

-define(DEFAULT_FIX, 60000).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([cluster_timestamp/0, local_timestamp/0]).

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, [], []).

cluster_timestamp() ->
	gen_server:call(?MODULE, {timestamp}).

local_timestamp() ->
	TS = {_,_, Micro} = os:timestamp(),
	Utc = calendar:now_to_universal_time(TS),
	Seconds = calendar:datetime_to_gregorian_seconds(Utc),
	((Seconds - 62167219200) * 1000000) + Micro. 

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {fix, fix_timer}).

%% init
init([]) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("~p [~p] is starting...\n", [?MODULE, self()]),
	FixInterval = application:get_env(cclock, fix_interval, ?DEFAULT_FIX),
	FixTimer = schedule(none, {send_fix}, FixInterval),
	initialize(),
	{ok, #state{fix=0, fix_timer=FixTimer}}.

%% handle_call
handle_call({timestamp}, _From, State=#state{fix=Fix}) ->
	Timestamp = timestamp(Fix),
	{reply, Timestamp, State}.

%% handle_cast
handle_cast(_Msg, State) ->
	{noreply, State}.

%% handle_info
handle_info({fix, RemoteTS}, State=#state{fix=Fix}) ->
	{_, NFix} = fix(RemoteTS, Fix),
	{noreply, State#state{fix=NFix}};

handle_info({fix, RemoteTS, From}, State=#state{fix=Fix}) ->
	{LocalTS, NFix} = fix(RemoteTS, Fix),
	Timestamp = timestamp(LocalTS, NFix),
	From ! fix_msg(Timestamp),
	{noreply, State#state{fix=NFix}};

handle_info({send_fix}, State=#state{fix=Fix}) ->
	Timestamp = timestamp(Fix),
	columbo:send_to_all(?MODULE, fix_msg(Timestamp)),
	{noreply, State}.

%% terminate
terminate(_Reason, #state{fix_timer=FixTimer}) ->
	timer:cancel(FixTimer),
	ok.

%% code_change
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

initialize() ->
	Timestamp = local_timestamp(),
	columbo:send_to_all(?MODULE, {fix, Timestamp, self()}).

fix(RemoteTS, Fix) ->
	LocalTS = local_timestamp(),
	Dif = RemoteTS - LocalTS,
	if Dif > Fix -> {LocalTS, Dif};
		true -> {LocalTS, Fix}
	end.

timestamp(Fix) ->
	timestamp(local_timestamp(), Fix).

timestamp(Timestamp, Fix) ->
	Timestamp + Fix.

fix_msg(Timestamp) ->
	{fix, Timestamp}.

schedule(none, Msg, Interval) ->
	{ok, NewTimer} = timer:send_interval(Interval, Msg),
	NewTimer;	
schedule(Timer, Msg, Interval) ->
	timer:cancel(Timer),
	schedule(none, Msg, Interval).
