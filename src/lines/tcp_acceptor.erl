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

%% @doc	Allows incoming tcp connections.
%%		Processes used as acceptors in line configuration needs to implement
%%		start_link/1 and activate/1 functions,
%%		sent by the line_proxy process when the corresponding interface is ready
%%		to work.
%%
%%		This acceptor spawns child processes to delegate receive and send operations.
%% @end

-module(tcp_acceptor).

-behaviour(gen_fsm).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1, activate/1]).

%%--------------------------------------------------------------------
%% Internal exports - gen_fsm callbacks
%%--------------------------------------------------------------------
-export([init/1, wait_activation/2, accepting/2, handle_event/3,
		handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-define(REGISTER_TIMEOUT, 10000).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {
			interface,
			module,
			port,
			socket,
			acceptor,
			conn_sup_name,
			conn_opt,
			options = []
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
%%		configuration for the acceptor. Specific parameters are
%%		<ul>
%%		<li>`{interface, Name}'		Name of the interface to serve</li>
%%		<li>`{module, Connection}'	module spawned to handle the connection</li>
%%		<li>`{timeout, Timeout}'	idle Timeout passed to the spawned connection</li>
%%		<li>`{port, Port}' 			Port used to listen for incoming connection</li>
%%		</ul>
%%		Other paramters are assumed to be options for gen_tcp:listen.
%% @end	
%%--------------------------------------------------------------------
start_link(Params) ->
    gen_fsm:start_link(?MODULE, Params, []).

%%--------------------------------------------------------------------
%% @spec	(Pid) -> ok
%%
%%				Pid	= pid()
%%
%% @doc		Activates the acceptor. Called by line_proxy.
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
	process_flag(trap_exit, true),
	State = initialize_state(Params),
	ok = line_proxy:register_acceptor(State#state.interface,
										?MODULE,
										State#state.module),
    {ok, wait_activation, State, ?REGISTER_TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec	(timeout, State) ->
%%				{next_state, wait_activation, State, Timeout}
%%
%% @doc     Repeats registration in line_proxy if the process has not been activated.
%% @hidden
%% @end
%%--------------------------------------------------------------------
wait_activation(timeout, State) ->
	ok = line_proxy:register_accceptor(State#state.interface,
										?MODULE,
										State#state.module),
    {next_state, wait_activation, State, ?REGISTER_TIMEOUT};

%%--------------------------------------------------------------------
%% @spec	(activate, State) ->
%%				{next_state, accepting, State, Timeout} |
%% 				{stop, Reason, State}
%%
%% @doc     Receives the activation message from line_proxy and
%% 			starts accepting connections.
%% @hidden
%% @end
%%--------------------------------------------------------------------
wait_activation(activate, State) ->
    case gen_tcp:listen(State#state.port, State#state.options) of
	    {ok, Socket} ->
			{ok, Ref} = prim_inet:async_accept(Socket, -1),
		    error_logger:info_msg("Line ~p is active: ~p ~p ~p~n",
		    						[State#state.interface,	?MODULE, State#state.port, State#state.options]),
			{next_state, accepting, State#state{socket=Socket, acceptor=Ref}};
		{error, Reason} ->
			{stop, Reason, State}
    end.

%%--------------------------------------------------------------------
%% @spec	(Event, State) ->
%%				{next_state, accepting, State}
%%
%% @doc     Handles activate message while being already activated.
%% @hidden
%% @end
%%--------------------------------------------------------------------
accepting(activate, State) ->
    {next_state, accepting, State}.

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

%% @clear

%%--------------------------------------------------------------------
%% @spec	(SocketData, accepting, State) ->
%%				{next_state, accepting, NextState}			|
%%				{stop, Reason, NewState}
%%
%% @doc     Accepts connections.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({inet_async, Socket, Ref, {ok, ClientSocket}}, accepting,
			#state{module=Module, socket=Socket, acceptor=Ref,
					conn_sup_name=SupName, conn_opt=Params} = State) ->
    case set_sockopt(Socket, ClientSocket) of
	    ok ->
    	    %% New client connected - spawn a new process using the simple_one_for_one supervisor.
    	    case supervisor:start_child(SupName, [Params]) of
    	    	{ok, Pid} when is_pid(Pid) ->
    	    		Module:set_port(Pid, ClientSocket);
    	    	{ok, Pid, _} when is_pid(Pid) ->
    	    		Module:set_port(Pid, ClientSocket)
    	    end,
	        %% Signal the network driver that we are ready to accept another connection
    	    {ok, NewRef} = prim_inet:async_accept(Socket, -1),
	        {next_state, accepting, State#state{acceptor=NewRef}};
	    {error, Reason} ->
    	    error_logger:error_msg("Error setting socket options: ~p.\n", [Reason]),
	        {stop, Reason, State}
    end;

handle_info({inet_async, Socket, Ref, Error}, accepting,
			#state{socket=Socket, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};

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
terminate(_Reason, _StateName, State) ->
    (catch gen_tcp:close(State#state.socket)),
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
%%			Strip acceptor and connection parameters, all others are assumed valid tcp options.
%% @hidden
%% @end
%%--------------------------------------------------------------------
initialize_state(Params) ->
	Interface = config_server:get_attr_value(interface, Params),
	Module = config_server:get_attr_value(module, Params),
	Port = config_server:get_attr_value(port, Params),
	Timeout = config_server:get_attr_value(timeout, Params),
	Options = lists:subtract(Params, [{interface,Interface},{module,Module},{port,Port},{timeout,Timeout}]),
	ConnectionSupName = list_to_atom(lists:flatten([Interface, "_conn_sup"])),
	#state {
		interface		= list_to_atom(Interface),
		module 			= list_to_atom(Module),
		port			= list_to_integer(Port),
		conn_sup_name	= ConnectionSupName,
		conn_opt		= [{interface, list_to_atom(Interface)},{timeout,list_to_integer(Timeout)}],
		options			= lists:map(fun parse_params/1, Options)
	}.

%%--------------------------------------------------------------------
%% @spec	(Param) -> {Key, Value2}
%%
%%				Param 	= {Key, Value}
%%				Key		= atom()
%%				Value	= string()
%%				Value2 	= int() | atom() | ip_address() "parsed parameters"
%%
%% @doc     Convert string values according parameter Key.
%% @hidden
%% @end
%%--------------------------------------------------------------------
parse_params({Any,[]}) ->
	Any;
parse_params({ip, String}) when is_list(String) ->
	{ok, Value} = inet_parse:address(String),
	{ip, Value};
parse_params({Key, Value}) when is_list(Value) ->
	case all_digits(Value) of
		true	-> {Key, list_to_integer(Value)};
		false	-> {Key, list_to_atom(Value)}
	end.

%%--------------------------------------------------------------------
%% @spec	(List) -> true | false
%%
%%				List	= string()
%%
%% @doc     Checks if the string contains only numeric digits.
%% @hidden
%% @end
%%--------------------------------------------------------------------
all_digits(List) when is_list(List) ->
	lists:all(fun
				(C) when C >= $0 andalso C =< $9 -> true;
				(_) -> false
				end, List).

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos, packet]) of
		{ok, Opts} ->
			case prim_inet:setopts(CliSocket, Opts) of
        		ok    -> ok;
		        Error -> gen_tcp:close(CliSocket), Error
        	end;
		Error ->
        	gen_tcp:close(CliSocket), Error
    end.
 
