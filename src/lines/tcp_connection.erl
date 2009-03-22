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

%% @doc	Handles tcp connection created by {@link tcp_acceptor} and {@link tcp_connector}.
%%		Each connection is handled by a tcp_connection process.
%%      Processes used as connection in line configuration needs to implement 
%%		send/1,	called by the interface to send messages to remote interfaces.
%%		Receive operations are implemented within the connection process.
%% @end
-module(tcp_connection).

-behaviour(gen_fsm).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1, set_port/2, send/2, connect_then_send/2]).

%%--------------------------------------------------------------------
%% Internal exports - gen_fsm callbacks
%%--------------------------------------------------------------------
-export([init/1, disconnected/2, connected/2, handle_event/3,
		handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("internal_msg.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {
                socket,		%% client socket
                addr,		%% client address
				timeout,
				interface,
				msg,		%% used to hold routing data
				retry = 3,	%% try to connect 3 times
				call_delay = 1000, %% wait 1 second before connect retry
				ip,			%% remote address
				port,		%% remote port
				tcp_opt = []
               }).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Params) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Params 	= list()
%%				Pid 	= pid()
%%
%% @doc	Starts the process.
%%		Params are passed by acceptor and connector processes.
%%		Specific parameters are
%%		<ul>
%%		<li>`{interface, Name}'		Name of the interface to serve</li>
%%		<li>`{timeout, Timeout}'	idle Timeout, when expires the connection closes</li>
%%		</ul>
%% @end
%%--------------------------------------------------------------------
start_link(Params) when is_list(Params) ->
    gen_fsm:start_link(?MODULE, Params, []).

%%--------------------------------------------------------------------
%% @spec	(Pid, Socket) -> ok
%%
%%				Pid 	= pid()
%%				Socket 	= port()
%%
%% @doc		Transfer the Socket port from the calling process to the server.
%%			tcp_acceptor uses this function to give ownership of the socket handle.
%% @end
%%--------------------------------------------------------------------
set_port(Pid, Socket) ->
	gen_tcp:controlling_process(Socket, Pid),
	gen_fsm:send_event(Pid, {connected, Socket}).

%%--------------------------------------------------------------------
%% @spec	(Pid, Msg) -> ok
%%
%% 				Pid = pid()
%% 				Msg = binary()
%%
%% @doc		Sends the message on the connection.
%% @end
%%--------------------------------------------------------------------
send(Pid, Msg) when is_pid(Pid) ->
	gen_fsm:send_event(Pid, {send, Msg}).

%%--------------------------------------------------------------------
%% @spec	(Pid, Msg) -> ok
%%
%% 				Pid = pid()
%% 				Msg = binary()
%%
%% @doc		Connects and sends the message on the connection.
%%			tcp_connector delegates the connect phase.
%% @end
%%--------------------------------------------------------------------
connect_then_send(Pid, Msg)	when is_pid(Pid) ->
	gen_fsm:send_event(Pid, {connect, Msg}).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Args) ->
%%				{ok, StateName, State}			|
%%				{ok, StateName, State, Timeout}	|
%%				ignore							|
%%				{stop, Reason}
%%
%% @doc     Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%%			gen_fsm:start_link/3,4, this function is called by the new
%%			process to initialize.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(Params) ->
	State = initialize_state(Params),
    {ok, disconnected, State}.

%%--------------------------------------------------------------------
%% @spec	({connected, Socket}, State) ->
%%				{next_state, connected, NextState, Timeout}
%%
%% @doc     The process receives a connected Socket.
%% @hidden
%% @end
%%--------------------------------------------------------------------
disconnected({connected, Socket}, #state{timeout=T} = State) ->
    {ok, {IP, _}} = inet:peername(Socket),
    inet:setopts(Socket, [{active, once}]),
    {next_state, connected, State#state{socket=Socket, addr=IP}, T};

%%--------------------------------------------------------------------
%% @spec	({connect, Msg}, State) ->
%%				{next_state, connected, NextState, Timeout}		|
%%				{next_state, disconnected, NextState, Timeout}
%%
%% @doc     The process receives a connected Socket.
%% @hidden
%% @end
%%--------------------------------------------------------------------
disconnected({connect, Msg}, State) ->
	do_connect(State#state{msg=Msg});

%%--------------------------------------------------------------------
%% @spec	(timeout, State) ->	{stop, normal, State}
%%
%% @doc     Retry timeout has expired and no more retry are available,
%% 			the process will stop.
%% @hidden
%% @end
%%--------------------------------------------------------------------
disconnected(timeout, #state{retry=0} = State) ->
	{stop, normal, State};

%%--------------------------------------------------------------------
%% @spec	(timeout, State) ->
%%				{next_state, connected, NextState, Timeout}		|
%%				{next_state, disconnected, NextState, Timeout}
%%
%% @doc     Retry timeout has expired, try to connect to remote host.
%% @hidden
%% @end
%%--------------------------------------------------------------------
disconnected(timeout, State) ->
	do_connect(State).

%%--------------------------------------------------------------------
%% @spec	(timeout, State) -> {stop, normal, State}
%%
%% @doc     Idle timeout has expired, the process will stop.
%% @hidden
%% @end
%%--------------------------------------------------------------------
connected(timeout, State) ->
	{stop, normal, State};

%%--------------------------------------------------------------------
%% @spec	({send, line_down}, State) -> {stop, normal, State}
%%
%% @doc     The process receives instructions to stop.
%% @hidden
%% @end
%%--------------------------------------------------------------------
connected({send, line_down}, State) ->
	{stop, normal, State};

%%--------------------------------------------------------------------
%% @spec	({send, Bin}, State) ->
%%				{next_state, connected, NextState, Timeout}
%%
%% @doc     Sends the message to the connection.
%% @hidden
%% @end
%%--------------------------------------------------------------------
connected({send, Bin}, #state{socket=Sock,timeout=T} = State) ->
	ok = gen_tcp:send(Sock, Bin),
	{next_state, connected, State, T}.

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
%%				{next_state, NextStateName, NextState, Timeout}	|
%%				{stop, Reason, NewState}
%%
%% @doc     Used to get incoming data from the tcp process.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, connected,
			#state{interface=Name, timeout=T} = StateData)
	when is_binary(Bin) ->
	iso_handler:recv({Name, self()}, Bin),
	% Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
	{next_state, connected, StateData,T};

handle_info({tcp_closed, _Socket}, _StateName, StateData) ->
    {stop, normal, StateData};

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
terminate(_Reason, _StateName, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
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
%% @hidden
%% @end
%%--------------------------------------------------------------------
initialize_state(Params) ->
	Interface = config_server:get_attr_value(interface, Params),
	Timeout = config_server:get_attr_value(timeout, Params),

	Host =
	case lists:keysearch(ip, 1, Params) of
		{value, {_,V1}} -> V1;
		_ -> undefined
	end,

	Port =
	case lists:keysearch(port, 1, Params) of
		{value, {_,V2}} -> V2;
		_ -> undefined
	end,
	
	Retry =
	case lists:keysearch(retry, 1, Params) of
		{value, {_,V3}} -> V3;
		_ -> 3
	end,

	Delay =
	case lists:keysearch(call_delay, 1, Params) of
		{value, {_,V4}} -> V4;
		_ -> 1000
	end,
	
	Options = lists:filter(
		fun ({K,_}) -> 
			not(lists:member(K,[interface, module, timeout, ip, port, retry, call_delay]));
			 (K) when is_atom(K) ->
			not(lists:member(K,[interface, module, timeout, ip, port, retry, call_delay]))
		end, Params),

	#state {
		interface		= Interface,
		timeout			= Timeout,
		msg				= undefined,
		ip				= Host,
		port			= Port,
		retry			= Retry,
		call_delay		= Delay,
		tcp_opt			= Options
	}.

%%--------------------------------------------------------------------
%% @spec	(State) ->
%%				{next_state, connected, NextState, Timeout}		|
%%				{next_state, disconnected, NextState, Timeout}
%%
%% @doc     Connects to the remote host.
%% @hidden
%% @end
%%--------------------------------------------------------------------
do_connect(#state{timeout=T, retry=R, ip=Host, port=Port, tcp_opt=Opt, msg=undefined} = State) ->
	case gen_tcp:connect(Host, Port, Opt) of
		{ok, Sock} -> 
			{ok, {IP, _}} = inet:peername(Sock),
			inet:setopts(Sock, [{active, once}]),
			{next_state, connected, State#state{socket=Sock, addr=IP},T};
		{error, _Reason} ->
			{next_state, disconnected, State#state{retry=R-1}, State#state.call_delay}
	end;

%%--------------------------------------------------------------------
%% @spec	(State) ->
%%				{next_state, connected, NextState, Timeout}		|
%%				{next_state, disconnected, NextState, Timeout}
%%
%% @doc     Connects to the remote host and sends the pending message.
%% @hidden
%% @end
%%--------------------------------------------------------------------
do_connect(#state{timeout=T, retry=R, ip=Host, port=Port, tcp_opt=Opt, msg=Bin} = State) ->
	case gen_tcp:connect(Host, Port, Opt) of
		{ok, Sock} -> 
			{ok, {IP, _}} = inet:peername(Sock),
			ok = gen_tcp:send(Sock, Bin),
			inet:setopts(Sock, [{active, once}]),
			{next_state, connected, State#state{socket=Sock, addr=IP},T};
		{error, _Reason} ->
			{next_state, disconnected, State#state{retry=R-1}, State#state.call_delay}
	end.

