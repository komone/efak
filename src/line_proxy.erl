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

%% @doc Connects interfaces to lines
%% @end

-module(line_proxy).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0, register_interface/1, register_connector/3,
		register_acceptor/3, send/1]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("internal_msg.hrl").

-define(LINKEDPIDS, line_proxy_pid_tab).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(line, {
			type,		%% {Type, InterfaceName}
						%% 		Type = acceptor | connector | interface
						%%		InterfaceName = atom()
			module,		%% module implementing the connection strategy
			pid,		%% pid() of the connection strategy process
			connection	%% module implementing the connection process
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
%% @spec	(Name) -> ok
%%
%%				Name = atom()
%%
%% @doc		Registers the interface named Name.
%%			`line_proxy' will inform all connected lines to activate.
%% @end	
%%--------------------------------------------------------------------
register_interface(Name) ->
	gen_server:cast(?MODULE, #line{ type = {interface, Name} }).

%%--------------------------------------------------------------------
%% @spec	(Interface, Module, ConnectionModule) -> ok
%%
%%				Interface 			= atom()
%%				Module 				= atom()
%%				ConnectionModule	= atom()
%%
%% @doc		Registers a line connector process serving the interface
%% 			named Interface.
%%
%% 			Module is the module implementing the connector,
%% 			ConnectionModule is the module implementing the connection.
%% @end	
%%--------------------------------------------------------------------
register_connector(Interface, Module, ConnectionModule) ->
	Line = #line{
		type = {connector, Interface},
		module = Module,
		pid = self(),
		connection = ConnectionModule
	},
	gen_server:abcast(?MODULE, Line),
	ok.

%%--------------------------------------------------------------------
%% @spec	(Interface, Module, ConnectionModule) -> ok
%%
%%				Interface 			= atom()
%%				Module 				= atom()
%%				ConnectionModule	= atom()
%%
%% @doc		Registers a line acceptor process serving the interface
%% 			named Interface.
%%
%% 			Module is the module implementing the acceptor,
%% 			ConnectionModule is the module implementing the connection.
%% @end	
%%--------------------------------------------------------------------
register_acceptor(Interface, Module, ConnectionModule) ->
	Line = #line{
		type = {acceptor, Interface},
		module = Module,
		pid = self(),
		connection = ConnectionModule
	},
	gen_server:abcast(?MODULE, Line),
	ok.

%%--------------------------------------------------------------------
%% @spec	(Msg) -> ok
%%
%%				Msg = #imsg{}
%%
%% @doc		Sends the message Msg to the destination line.
%% @hidden
%% @end
%%--------------------------------------------------------------------
send(#imsg{cmd=data,dst={Name, Pid},msg=Bin}) when is_pid(Pid) ->
	[#line{connection=Module} | _] = registered_lines(Name),
	Module:send(Pid, Bin);

send(#imsg{cmd=data,dst={Name, undefined},msg=Bin}) ->
	Connectors = ets:lookup(?MODULE, {connector,Name}),
	#line{module=Module, pid=Pid} =
	lists:nth(erlang:phash2(now(), length(Connectors))+1, Connectors),
	Module:connect_then_send(Pid, Bin);

send(#imsg{cmd=line_down,dst={Name, Pid}}) when is_pid(Pid) ->
	[#line{connection=Module} | _] = registered_lines(Name),
	Module:send(Pid, line_down).

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
%% @doc     Initiates the server. Creates the connection table
%%			to keep track of the registered lines.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	ConnTab = ets:new(?MODULE, [named_table, duplicate_bag, {keypos, 2}]),
	LinkedPid = ets:new(?LINKEDPIDS, [set, {keypos, 4}]),
	load_remote_lines({ConnTab, LinkedPid}),
	{ok, {ConnTab, LinkedPid}}.

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
handle_call(registered_lines, _From, State) ->
	Reply = {ok, registered_lines()},
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

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
handle_cast(#line{type={interface,_}} = Line, {ConnTab, _} = State) ->
	ets:insert_new(ConnTab, Line),
	%% when an interface is registered try activating connected lines
	activate_lines(),
	{noreply, State};

handle_cast(#line{} = Line, State) ->
	register_line(Line, State),
	%% when a new line is registered try activating connected lines
	activate_lines(),
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
handle_info({'EXIT', Pid, _Reason}, {ConnTab, LinkedPid} = State) ->
	case ets:lookup(LinkedPid, Pid) of
		[Line] ->
			ets:delete_object(LinkedPid, Line),
			ets:delete_object(ConnTab, Line);
		_ -> ok
	end,
	{noreply, State};

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
terminate(_Reason, {ConnTab, LinkedPid}) ->
	ets:delete(ConnTab),
	[ unlink(X#line.pid) || X <- ets:tab2list(LinkedPid)],
	ets:delete(LinkedPid),
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

%%--------------------------------------------------------------------
%% @spec	() -> ok
%%
%% @doc		Activates lines for all registered interfaces.
%%
%%			Line processes (connectors/acceptors) are activated
%%			calling Module:activate/0.
%% @hidden
%% @end
%%--------------------------------------------------------------------
activate_lines() ->
	Interfaces = registered_interfaces(),
	[M:activate(Pid) || I <- Interfaces,
		#line{module=M,pid=Pid} <- ets:lookup(?MODULE, {acceptor,I})],
	[M:activate(Pid) || I <- Interfaces,
		#line{module=M,pid=Pid} <- ets:lookup(?MODULE, {connector,I})],
	ok.

%%--------------------------------------------------------------------
%% @spec	() -> [Interface]
%%
%% 				Interface = atom()
%%
%% @doc		Returns the name list of the registered interfaces.
%% @hidden
%% @end
%%--------------------------------------------------------------------
registered_interfaces() ->
	lists:flatten(
		ets:match(?MODULE, #line{type={interface,'$1'},_='_'})
	).

%%--------------------------------------------------------------------
%% @spec	() -> [Line]
%%
%% 				Line = #line{}
%%
%% @doc		Returns the registered lines (acceptors/connectors).
%% @hidden
%% @end
%%--------------------------------------------------------------------
registered_lines() ->
	lists:flatten([
		ets:match_object(?MODULE, #line{type={acceptor,'_'},_='_'}),
		ets:match_object(?MODULE, #line{type={connector,'_'},_='_'})
	]).

%%--------------------------------------------------------------------
%% @spec	(Interface) -> [Line]
%%
%% 				Line = #line{}
%%
%% @doc		Returns the registered lines (acceptors/connectors) for
%% 			the interface named Interface.
%% @hidden
%% @end
%%--------------------------------------------------------------------
registered_lines(Interface) ->
	lists:flatten([
		ets:lookup(?MODULE, {acceptor,Interface}),
		ets:lookup(?MODULE, {connector,Interface})
	]).

%%--------------------------------------------------------------------
%% @spec	(Line, {ConnTab, LinkedPid}) -> ok
%%
%% 				Line = #line{}
%%
%% @doc		Adds the Line to the tables and links to the line process.
%% @hidden
%% @end
%%--------------------------------------------------------------------
register_line(Line, {ConnTab, LinkedPid}) ->
	case ets:insert_new(LinkedPid, Line) of
		true ->
			ets:insert(ConnTab, Line),
			true = link(Line#line.pid),
			ok;
		false ->
			ok
	end.

%%--------------------------------------------------------------------
%% @spec	(Tables) -> ok
%%
%% @doc		Tries to load known lines from other nodes.
%% @hidden
%% @end
%%--------------------------------------------------------------------
load_remote_lines(Tables) ->
	{Replies,_} = gen_server:multi_call(nodes(), ?MODULE, registered_lines),
	[register_line(L, Tables) || {ok, Lines} <- Replies, L <- Lines],
	ok.
	
