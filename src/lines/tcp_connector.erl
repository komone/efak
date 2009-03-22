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

%% @doc	Allows outgoing tcp connections.
%%		Processes used as connectors in line configuration needs to implement 
%%		start_link/1, connect_then_send/2 and activate/1, sent by the
%% 		line_proxy process when the corresponding interface is ready
%%		to work.
%%
%%		Connector spawns child processes to delegate receive and send operations.
%% @end

-module(tcp_connector).

-behaviour(gen_fsm).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1, connect_then_send/2, activate/1]).

%%--------------------------------------------------------------------
%% Internal exports - gen_fsm callbacks
%%--------------------------------------------------------------------
-export([init/1, connecting/2, wait_activation/2, handle_event/3,
		handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-define(REGISTER_TIMEOUT, 10000).

-include("internal_msg.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {
			interface,
			module,
			conn_sup_name,
			conn_opt			
		}).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Params) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Params	= [{Key,Value}]
%%				Key		= atom()
%%				Value	= string()
%%				Pid		= pid()
%%
%% @doc	Starts the server.
%%		Params are passed by the line_sup process after reading the
%%		configuration for the connector. Specific parameters are
%%		<ul>
%%		<li>`{interface, Name}'		Name of the interface to serve</li>
%%		<li>`{module, Connection}'	module spawned to handle the connection</li>
%%		<li>`{timeout, Timeout}'	idle Timeout passed to the spawned connection</li>
%%		<li>`{ip, Address}' 		Address to connect to</li>
%%		<li>`{port, Port}' 			Port to connect to</li>
%%		<li>`{retry, Retry}'		number of Retry used to connect (default 3)</li>
%%		<li>`{call_delay, MSec}'	MSec between connection retries (default 1000)</li>
%%		</ul>
%%		Other paramters are assumed to be options for gen_tcp:connect.
%% @end	
%%--------------------------------------------------------------------
start_link(Params) ->
    gen_fsm:start_link(?MODULE, Params, []).

%%--------------------------------------------------------------------
%% @spec	(Pid, Msg) -> ok
%%
%%				Pid	= pid()
%%				Msg	= binary()
%%
%% @doc		Connects to remote host and send the message Msg.
%%			line_proxy process calls this function when a new connection is
%%			required.
%% @end	
%%--------------------------------------------------------------------
connect_then_send(Pid, Msg) when is_pid(Pid) ->
	gen_fsm:send_event(Pid, {connect, Msg}).

%%--------------------------------------------------------------------
%% @spec	(Pid) -> ok
%%
%%				Pid	= pid()
%%
%% @doc		Activates the connector. Called by line_proxy.
%% @end	
%%--------------------------------------------------------------------
activate(Pid) ->
	gen_fsm:send_event(Pid, activate).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Params) ->
%%				{ok, StateName, State}			|
%%				{ok, StateName, State, Timeout}	|
%%				ignore							|
%%				{stop, Reason}
%%
%%				Params	= [{Key,Value}]
%%				Key		= atom()
%%				Value	= string()
%%
%% @doc     Initialize the server and tries to register.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(Params) ->
	State = initialize_state(Params),
	ok = line_proxy:register_connector(State#state.interface,
										?MODULE,
										State#state.module),
    {ok, wait_activation, State, ?REGISTER_TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec	(Event, State) ->
%%				{next_state, wait_activation, State, Timeout}
%%
%% @doc     Repeats registration in line_proxy if it has not been activated.
%% @hidden
%% @end
%%--------------------------------------------------------------------
wait_activation(timeout, State) ->
	ok = line_proxy:register_connector(State#state.interface,
										?MODULE,
										State#state.module),
    {next_state, wait_activation, State, ?REGISTER_TIMEOUT};

%%--------------------------------------------------------------------
%% @spec	(Event, State) ->
%%				{next_state, wait_activation, State, Timeout}
%%
%% @doc     Repeats registration in line_proxy if it has not been activated.
%% @hidden
%% @end
%%--------------------------------------------------------------------
wait_activation(activate, State) ->
    error_logger:info_msg("Line ~p is active: ~p ~p~n",	[State#state.interface,
    						?MODULE, State#state.conn_opt]),
	{next_state, connecting, State}.

%%--------------------------------------------------------------------
%% @spec	({connect, Msg}, State) ->
%%				{next_state, connecting, NextState}
%%
%% @doc     Delegates connection set-up to the connection module
%% 			because it can require lot of time.
%% @hidden
%% @end
%%--------------------------------------------------------------------
connecting({connect, Msg},
			 #state{module=Module, conn_sup_name=SupName, conn_opt=Params} = State) ->
    case supervisor:start_child(SupName, [Params]) of
    	{ok, Pid} when is_pid(Pid) ->
    		Module:connect_then_send(Pid, Msg);
    	{ok, Pid, _} when is_pid(Pid) ->
    		Module:connect_then_send(Pid, Msg)
    end,
    {next_state, connecting, State};

connecting(activate, State) ->
    {next_state, connecting, State}.

%%--------------------------------------------------------------------
%% @spec	(Event, StateName, State) ->
%%				{next_state, NextStateName, NextState}			|
%%				{next_state, NextStateName, NextState, Timeout}	|
%%				{stop, Reason, NewState}
%%
%% @doc     Whenever a gen_fsm receives an event sent using
%%			gen_fsm:send_all_state_event/2, this function is called
%%			to handle the event.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @spec	(Event, From, StateName, State) ->
%%				{next_state, NextStateName, NextState}				|
%%				{next_state, NextStateName, NextState, Timeout}		|
%%				{reply, Reply, NextStateName, NextState}			|
%%				{reply, Reply, NextStateName, NextState, Timeout}	|
%%				{stop, Reason, NewState}							|
%%				{stop, Reason, Reply, NewState}
%%
%% @doc     Whenever a gen_fsm receives an event sent using
%%			gen_fsm:sync_send_all_state_event/2,3, this function is
%%			called to handle the event.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @spec	(Info, StateName, State) ->
%%				{next_state, NextStateName, NextState}			|
%%				{next_state, NextStateName, NextState, Timeout}	|
%%				{stop, Reason, NewState}
%%
%% @doc     This function is called by a gen_fsm when it receives any
%%			other message than a synchronous or asynchronous event
%%			(or a system message).
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @spec	(Reason, StateName, State) -> void()
%%
%% @doc     This function is called by a gen_fsm when it is about
%%			to terminate. It should be the opposite of Module:init/1
%%			and do any necessary cleaning up. When it returns, the
%%			gen_fsm terminates with Reason. The return value is ignored.
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec	(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%%
%% @doc     Convert process state when code is changed.
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Params) -> State
%%
%%				Params	= [{Key,Value}]
%%				Key		= atom()
%%				Value	= string()
%%				State 	= #state{}
%%
%% @doc     Initialize state form Params list.
%%			Strip acceptor and connection parameters, all others are
%% 			assumed valid tcp options.
%% @hidden
%% @end
%%--------------------------------------------------------------------
initialize_state(Params) ->
	Interface = config_server:get_attr_value(interface, Params),
	Module = config_server:get_attr_value(module, Params),
	ConnectionSupName = list_to_atom(lists:flatten([Interface, "_conn_sup"])),
	Options = lists:subtract(Params, [{module,Module}]),
	#state {
		interface		= list_to_atom(Interface),
		module 			= list_to_atom(Module),
		conn_sup_name	= ConnectionSupName,
		conn_opt		= [config_server:parse_params(X) || X <- Options]
	}.

