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

%%
%% @doc	The erl_interface lets erlang programs to start transactions
%% 		in an application that uses efak.
%%
%%      To configure an erl_interface look at {@link erl_interface_sup}.
%% @end

-module(erl_interface).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/2, cast/1, call/1, call/2, reply/2]).

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
			name,
			callback
		}).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Name, Callback) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Pid = pid()
%%
%% @doc		Starts the server
%% @end	
%%--------------------------------------------------------------------
start_link(Name, Callback) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Name, Callback], []).

%%--------------------------------------------------------------------
%% @spec	(Request) -> ok
%%
%% @doc		Starts a transaction passing the Request message.
%%
%%			The message delivered to the transaction is
%% 			`{cast, Request}'.
%% @end	
%%--------------------------------------------------------------------
cast(Request) ->
	gen_server:cast(?MODULE, Request).

%%--------------------------------------------------------------------
%% @spec	(Request) -> Reply
%%
%%				Reply = term()
%%
%% @doc		Starts a transaction passing the Request message and
%% 			wait for the transaction reply.
%%
%%			The message delivered to the transaction is
%% 			`{call, {From, Request}}'.
%%
%%			The transaction can call erl_interface:reply/1 to send 
%% 			the reply message.
%% @end	
%%--------------------------------------------------------------------
call(Request) ->
	gen_server:call(?MODULE, Request).

%%--------------------------------------------------------------------
%% @spec	(Request, Timeout) -> ok
%%
%%				Timeout = integer() | infinity
%%
%% @doc		Starts a transaction passing the Request message and
%% 			wait for the transaction reply for Timeout milliseconds.
%%
%%			The message delivered to the transaction is
%% 			`{call, {From, Request}}'.
%%
%%			The transaction can call erl_interface:reply/1 to send 
%% 			the reply message.
%% @end	
%%--------------------------------------------------------------------
call(Request, Timeout) ->
	gen_server:call(?MODULE, Request, Timeout).

%%--------------------------------------------------------------------
%% @spec	(From, Reply) -> ok
%%
%% @doc		Replies to the process that started the transaction
%% 			with erl_interface:call/1.
%% @end	
%%--------------------------------------------------------------------
reply({call, From}, Reply) ->
	gen_server:reply(From, Reply);

reply(From, Reply) ->
	gen_server:reply(From, Reply).

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
init([Name, Callback]) ->
	{ok, #state{name=Name, callback=Callback}}.

%%--------------------------------------------------------------------
%% @spec	handle_call(Request, From, State) -> {noreply, State}
%%
%% @doc		Handling call messages
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Request, From, #state{name=Name,callback={M,F,A}} = State) ->
	Result = apply(M, F, A ++ [{{Name,undefined}, {call, {From, Request}}}]),
	case Result of
		{error, no_transaction} -> {reply, {error, no_transaction}, State};
		_ -> {noreply, State}
	end.

%%--------------------------------------------------------------------
%% @spec	handle_cast(Msg, State) -> {noreply, State}
%%
%% @doc		Handling cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, #state{name=Name,callback={M,F,A}} = State) ->
	apply(M, F, A ++ [{{Name,undefined}, {cast, Msg}}]),
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
terminate(_Reason, _State) ->
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

