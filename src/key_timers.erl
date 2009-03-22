%% efak - erlang financial application kit
%% Copyright (C) 2009  Paolo Montrasi
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

%% @doc	Associates Keys to timers.
%%
%%		User can manipulate timers using arbitrary keys within all
%% 		connected nodes.
%% @end

-module(key_timers).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0, add/2, remove/1]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {
			keytab
		}).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	() -> {ok, Pid} | ignore | {error, Error}
%%
%%				Pid = pid()
%%
%% @doc		Starts the server
%% @end	
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec	(Key, {ok, TRef}) -> ok
%%
%%				Key  = term()
%%				TRef = tref()
%%
%% @doc		Adds a new keyed timer in the local node.
%% 			Key is expected to be unique.
%% @end	
%%--------------------------------------------------------------------
add(Key, {ok, TRef}) ->
	gen_server:call(?MODULE, {add, Key, TRef});

add(_Key, {error, _}) ->
	ok.

%%--------------------------------------------------------------------
%% @spec	(Key) -> ok
%%
%%				Key  = term()
%%				TRef = tref()
%%
%% @doc		Removes the keyed timer from any connected nodes.
%% @end	
%%--------------------------------------------------------------------
remove(Key) ->
	gen_server:abcast(?MODULE, {remove, Key}).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Args) ->
%%				{ok, State}				|
%%				{ok, State, Timeout}	|
%%				ignore					|
%%				{stop, Reason}
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([]) ->
	Tab = ets:new(?MODULE, [set, {keypos, 1}]),
	{ok, #state{keytab = Tab}}.

%%--------------------------------------------------------------------
%% @spec	handle_call(Request, From, State) ->
%%				{reply, Reply, State}			|
%%				{reply, Reply, State, Timeout}	|
%%				{noreply, State}				|
%%				{noreply, State, Timeout}		|
%%				{stop, Reason, Reply, State}	|
%%				{stop, Reason, State}
%%
%% @doc		Handling call messages
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({add, Key, TRef}, _From, #state{keytab=Tab} = State) ->
	ets:insert(Tab, {Key, TRef}),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @spec	handle_cast(Msg, State) ->
%%				{noreply, State}			|
%%				{noreply, State, Timeout}	|
%%				{stop, Reason, State}
%%
%% @doc		Handling cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({remove, Key}, #state{keytab=Tab} = State) ->
	case ets:lookup(Tab, Key) of
		[{Key, TRef}] ->
			timer:cancel(TRef),
			ets:delete(Tab, Key);
		_ -> ok
	end,
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @spec	handle_info(Msg, State) ->
%%				{noreply, State}			|
%%				{noreply, State, Timeout}	|
%%				{stop, Reason, State}
%%
%% @doc		Handling all non call/cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @spec	(Reason, State) -> term() "ignored by gen_server"
%%
%% @doc		This function is called by a gen_server when it is about
%%			to terminate. It should be the opposite of Module:init/1
%%			and do any necessary cleaning up. When it returns, the
%%			gen_server terminates with Reason.
%%			The return value is ignored.
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{keytab=Tab}) ->
	ets:delete(Tab),
	ok.

%%--------------------------------------------------------------------
%% @spec	(OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc		Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

