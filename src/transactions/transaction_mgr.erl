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
%% @doc Starts transaction processes when receiving new messages from
%%		the interfaces.
%%
%%		The transaction manager can be started in your application
%%		including it in a supervisor tree and adding the callback
%%		triplet `{transaction_mgr,start_transaction,[]}' in the
%%		interface configuration of your `efakconf.xml' file.
%%
%%		When a message is received the transcation manager will try
%%		to start a new process to handle the message flow.
%%
%% @see iso_transaction
%%

-module(transaction_mgr).

-behaviour(supervisor).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1, start_transaction/1]).

%%--------------------------------------------------------------------
%% Internal exports - supervisor callbacks
%%--------------------------------------------------------------------
-export([init/1]).

-define(TRANSACTION_TAB, transaction_controller).
-define(TRX_SUP, transactions_sup).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Transactions) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Transactions 	= [atom()]
%%				Pid 			= pid()
%%
%% @doc		Starts the transaction manager.
%%
%%			When starting it starts the efak application.
%%
%%			Transactions is a list of modules that implement the
%% 			{@link iso_transaction} behaviour.
%% @end	
%%--------------------------------------------------------------------
start_link(Transactions) ->
	Trx = lists:map(fun(X) -> {module, X} end, Transactions),
	supervisor:start_link({local, ?MODULE}, ?MODULE, Trx).

%%--------------------------------------------------------------------
%% @spec	({From, Msg}) -> ok | {error, no_transaction}
%%
%%				From	= dest()
%%				Msg		= isomsg() | {atom(), term()}
%%				Pid 	= pid()
%%
%% @doc		Starts a new transaction.
%%
%%			should_start/1 is called on all transaction modules to
%% 			identify the transaction to start.
%% @end
%%--------------------------------------------------------------------
start_transaction({{Interface,_} = From, Msg}) ->
	Result = ets:foldl(fun({module, Module}, L) ->
							case Module:should_start({Interface, Msg}) of
								true -> [Module | L];
								false -> L
							end
						end, [], ?TRANSACTION_TAB),
	start_child(Result, {From, Msg}).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Transactions) ->
%%				{ok,  {SupFlags,  [ChildSpec]}}	|
%%				ignore							|
%%				{error, Reason}
%%
%% @doc     Whenever a supervisor is started using 
%%			supervisor:start_link/[2,3], this function is called by
%%			the new process to find out about restart strategy,
%%			maximum restart frequency and child specifications.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(Transactions) ->
	application:start(efak),
	{ok,{{one_for_one,0,1}, [
		{?TRX_SUP,
			{simple_worker_sup,	start_link,	[?TRX_SUP, iso_transaction]},
			permanent, infinity, supervisor, [simple_worker_sup]},
		{transaction_controller,
			{transaction_controller,start_link,[Transactions]},
			permanent,5000,worker,[transaction_controller]}
	]}}.

%%====================================================================
%% Internal functions
%%====================================================================

start_child([], _) ->
	{error, no_transaction};

start_child([Module | _], {From, _} = Msg) ->
	{ok, Pid} = supervisor:start_child(?TRX_SUP, [Module, From]),
	ok = iso_transaction:send(Pid, Msg).

