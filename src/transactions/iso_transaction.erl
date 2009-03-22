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

%%--------------------------------------------------------------------
%% @doc	A behaviour to implement transactions that cooperate
%%		with the transaction manager and iso interfaces.
%%
%%	== Callbacks ==
%%
%%	=== should_start/1 ===
%%```
%% should_start({Interface, Msg}) -> true | false
%%
%%    Interface = atom()
%%    Msg = isomsg() | Command
%%    Command = {atom(), term()}
%%'''
%% 			Replies true if the transaction can process the message
%%			Msg coming from the interface named Interface.
%%
%%	=== init/0 ===
%%```
%% init() -> {ok, State}
%%'''
%% 			Initialize the transaction state.
%%
%%	=== request/2 ===
%%```
%% request({Interface, Request}, State) ->
%%     {request, {To, Request}, NewState}          |
%%     {request, {To, Request}, NewState, Timeout} |
%%     {advice, {To, Request}, Response, NewState} |
%%     {response, Response, NewState}              |
%%     {stop, NewState}
%%
%%    Interface = atom()
%%    Request   = isomsg()
%%    To        = atom()
%%    Timeout   = interger()
%%    Response  = isomsg()
%%'''
%% 			Receives the request message Request from interface named Interface.
%%
%%	=== response/3 ===
%%```
%% response({Interface, Response}, Request, State) ->
%%     {request, {To, Request}, NewState}          |
%%     {request, {To, Request}, NewState, Timeout} |
%%     {advice, {To, Request}, Response, NewState} |
%%     {response, Response, NewState}              |
%%     {stop, NewState}
%%
%%    Interface = atom()
%%    Request   = isomsg()
%%    To        = atom()
%%    Timeout   = interger()
%%    Response  = isomsg()
%%'''
%% 			Receives the response message Response from interface
%% 			named Interface. Request is the first message received
%%			by the transaction.
%%
%%	=== timeout/3 ===
%%```
%% timeout({Interface, LastRequest}, FirstRequest, State) ->
%%     {request, {To, Request}, NewState}          |
%%     {request, {To, Request}, NewState, Timeout} |
%%     {advice, {To, Request}, Response, NewState} |
%%     {response, Response, NewState}              |
%%     {stop, NewState}
%%
%%    Interface    = atom()
%%    LastRequest  = isomsg()
%%    FirstRequest = isomsg()
%%    Timeout      = interger()
%%'''
%% 			The transaction or the interface named Interface timed out
%% 			while waiting for the response to the LastRequest message.
%%			FirstRequest is the original request that started the transaction.
%%
%%	=== terminate/2 ===
%%```
%% terminate(Reason, State) -> ok
%%
%%    Reason = normal | line_down | timeout
%%'''
%% 			The transaction is going to stop.
%%
%%	=== command/2 ===
%%```
%% command({Interface, Command}, State) ->
%%      {request, {To, Request}, NewState}          |
%%      {request, {To, Request}, NewState, Timeout} |
%%      {advice, {To, Request}, NewState}           |
%%      {stop, NewState}
%%
%%     Interface = atom()
%%     Command   = {atom(), term()}
%%     Request   = isomsg()
%%     To        = atom()
%%     Timeout   = interger()
%%'''
%% 			The interface named Interface sends the Command.
%%
%%
%%	== Callback results ==
%%
%%				`{request, {To, Request}, NewState}'
%% <ol>
%% <li>sends the new Request to interface named To, the
%%     transaction will wait for a response</li>
%% </ol>
%%
%%```
%% Src interface     Transaction     Dst interface
%%
%%             request/2
%%         --------------->        1.
%%                           ------------>
%%'''
%%				`{advice, {To, Request}, Response, NewState}'
%% <ol>
%% <li>sends the new Request to interface named To via the saf
%%     process, the response will not be delivered to the
%%     transaction</li>
%% <li>sends the Response message for the receiving Request</li>
%% </ol>
%%
%%```
%% Src interface     Transaction     Dst interface
%%
%%             request/2
%%         --------------->        1.
%%                2.         ------------>
%%         <---------------
%%'''
%%
%%				`{response, Response, NewState}'
%% <ol>
%% <li>sends the Response message for the receiving Request</li>
%% </ol>
%%
%%```
%% Src interface     Transaction     Dst interface
%%
%%             request/2
%%         --------------->
%%                1.
%%         <---------------
%%'''
%%
%%				`{stop, NewState}' stops the transaction
%%
%% @end
%%--------------------------------------------------------------------

-module(iso_transaction).

-export([behaviour_info/1]).

-behaviour(gen_fsm).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/2, send/2, wait_request/2,
		wait_response/2]).

%%--------------------------------------------------------------------
%% Internal exports - gen_fsm callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
		terminate/3, code_change/4]).

-define(TIMEOUT, 60000).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {
				from,
				in_req,
				out_req,
				module,
				timeout = ?TIMEOUT,
				state
				}).

%% @hidden
behaviour_info(callbacks) ->
    [{should_start, 1},
     {init, 0},
     {request, 2},
     {response, 3},
     {timeout, 3},
     {terminate, 2},
     {command, 2}];
behaviour_info(_Other) ->
    undefined.

%%--------------------------------------------------------------------
%% @spec	(Module, From) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Module	= atom()
%%				From	= dest()
%%
%% @doc		The transaction is created by the transaction_mgr when a
%% 			message is received.
%%
%%			From is the transaction originating interface (the one to
%%			send responses to).
%%
%%			Module provides functions to process the transaction.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
start_link(Module, From) ->
    gen_fsm:start_link(?MODULE, [Module, From], []).

%%--------------------------------------------------------------------
%% @spec	(Pid, Msg) -> ok
%%
%%				Pid = pid()
%%				Msg = {dest(), isomsg()} | {dest(), {Error, isomsg()}}
%%
%% @doc		Sends the message Msg to the transaction.
%%
%% 			Used by the interface to deliver messages.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
send(Pid, {_,IsoMsg} = Msg) when is_pid(Pid), is_list(IsoMsg) ->
	gen_fsm:send_event(Pid, Msg);

send(Pid, {_, {ErrorOrCommand,_}} = Msg) when is_pid(Pid), is_atom(ErrorOrCommand) ->
	gen_fsm:send_event(Pid, Msg).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Params) -> {ok, StateName, State}
%%
%%				Params	= list()
%%				Module 	= atom()
%%				From 	= dest()
%%
%% @doc     Initialize the transaction with the module Module and
%%			the transaction originator From.
%%			The transaction stats in `wait_request' state.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([Module, From]) ->
	{ok, State} = Module:init(),
	{ok, wait_request, #state{ from = From, module = Module, state = State}}.

%% @hidden
handle_event(_Event, StateName, State) -> {next_state, StateName, State}.

%% @hidden
handle_sync_event(_Event, _From, StateName, State) -> {reply, ok, StateName, State}.

%% @hidden
handle_info(_Info, StateName, State) -> {next_state, StateName, State}.

%% @hidden
terminate(Reason, _StateName, #state{module=Mod, state=St}) ->
	Mod:terminate(Reason, St).

%% @hidden
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%%--------------------------------------------------------------------
%% @spec	({From, Msg}, State) ->
%% 				{next_state, wait_response, State, Timeout} |
%%				{stop, normal, State}
%%
%%				From	= dest()
%%				Msg		= isomsg()
%%
%% @doc     Receives the request message Msg and process it.
%%
%%			If a request is produced, it is sent to the destination 
%% 			interface and the state is changed in wait_response.
%%			If a response is produced, it is sent back to the From
%%			interface and the transaction stops.
%% @hidden
%% @end
%%--------------------------------------------------------------------
wait_request({{Interface,_} = From, {Command, _} = Msg},
				#state{from=From, module=Mod, state=St} = State)
	when is_atom(Command) ->
	case catch Mod:command({Interface, Msg}, St) of
		{request, {To, Request}, NewSt} ->
			iso_interface:send(To, Request, me()),
		    {next_state, wait_response,
		    State#state{in_req=Msg, out_req={To,Request}, state=NewSt},
		    State#state.timeout};
		{request, {To, Request}, NewSt, Timeout} ->
			iso_interface:send(To, Request, me()),
		    {next_state, wait_response,
		    State#state{in_req=Msg, out_req={To,Request}, state=NewSt},
		    Timeout};
		{advice, {To, Request}, NewSt} ->
			iso_interface:store_and_forward(To, Request),
		    {stop, normal, State#state{state=NewSt}};
		{stop, NewSt} ->
		    {stop, normal, State#state{state=NewSt}};
		{'EXIT', Reason} -> %% {'EXIT', Reason}
			exit(Reason)
	end;

wait_request({{Interface,_} = From, Msg},
				#state{from=From, module=Mod, state=St} = State) ->
	case catch Mod:request({Interface, Msg}, St) of
		{request, {To, Request}, NewSt} ->
			iso_interface:send(To, Request, me()),
		    {next_state, wait_response,
		    State#state{in_req=Msg, out_req={To,Request}, state=NewSt},
		    State#state.timeout};
		{request, {To, Request}, NewSt, Timeout} ->
			iso_interface:send(To, Request, me()),
		    {next_state, wait_response,
		    State#state{in_req=Msg, out_req={To,Request}, state=NewSt},
		    Timeout};
		{advice, {To, Request}, Response, NewSt} ->
			iso_interface:send(From, Response),
			iso_interface:store_and_forward(To, Request),
		    {stop, normal, State#state{state=NewSt}};
		{response, Response, NewSt} ->
			iso_interface:send(From, Response),
		    {stop, normal, State#state{state=NewSt}};
		{stop, NewSt} ->
		    {stop, normal, State#state{state=NewSt}};
		{'EXIT', Reason} -> %% {'EXIT', Reason}
			exit(Reason)
	end.

%%--------------------------------------------------------------------
%% @spec	({From, Msg}, State) ->
%% 				{next_state, wait_response, State, Timeout} |
%%				{stop, normal, State}
%%
%%				From	= dest()
%%				Msg		= isomsg() | {Error, isomsg()}
%%				Error	= line_down | timeout
%%
%% @doc     Receives a response from the remote interface and sends
%%			it back to the original request.
%% @hidden
%% @end
%%--------------------------------------------------------------------
%% Transaction timed out
wait_response(timeout,
			#state{from=From, in_req=Msg, out_req={Interface, Request},
					module=Mod, state=St} = State) ->
	case catch Mod:timeout({Interface, Request}, Msg, St) of
		{request, {To, NewRequest}, NewSt} ->
			iso_interface:send(To, NewRequest, me()),
		    {next_state, wait_response,
		    State#state{out_req={To,Request}, state=NewSt},
		    State#state.timeout};
		{request, {To, NewRequest}, NewSt, Timeout} ->
			iso_interface:send(To, NewRequest, me()),
		    {next_state, wait_response,
		    State#state{out_req={To,Request}, state=NewSt},
		    Timeout};
		{advice, {To, Request}, Response, NewSt} ->
			iso_interface:send(From, Response),
			iso_interface:store_and_forward(To, Request),
		    {stop, normal, State#state{state=NewSt}};
		{response, Response, NewSt} ->
			iso_interface:send(From, Response),
		    {stop, normal, State#state{state=NewSt}};
		{stop, NewSt} ->
		    {stop, normal, State#state{state=NewSt}};
		{'EXIT', Reason} -> %% {'EXIT', Reason}
			exit(Reason)
	end;

%% Error received from the remote interface
wait_response({{Interface,_}, {Error, Request}},
				#state{from=From, in_req=Msg, out_req={Interface,_},
						module=Mod, state=St} = State)
	when Error =:= line_down orelse Error =:= timeout orelse Error =:= down ->
	case catch Mod:timeout({Interface, Request}, Msg, St) of
		{request, {To, NewRequest}, NewSt} ->
			iso_interface:send(To, NewRequest, me()),
		    {next_state, wait_response,
		    State#state{out_req={To,Request}, state=NewSt},
		    State#state.timeout};
		{request, {To, NewRequest}, NewSt, Timeout} ->
			iso_interface:send(To, NewRequest, me()),
		    {next_state, wait_response,
		    State#state{out_req={To,Request}, state=NewSt},
		    Timeout};
		{advice, {To, Request}, Response, NewSt} ->
			iso_interface:send(From, Response),
			iso_interface:store_and_forward(To, Request),
		    {stop, normal, State#state{state=NewSt}};
		{response, Response, NewSt} ->
			iso_interface:send(From, Response),
		    {stop, normal, State#state{state=NewSt}};
		{stop, NewSt} ->
		    {stop, normal, State#state{state=NewSt}};
		{'EXIT', Reason} -> %% {'EXIT', Reason}
			exit(Reason)
	end;

%% Response received
wait_response({{Interface,_} = OutConn, Response},
				#state{from=From, in_req=Msg, out_req={Interface,_},
						module=Mod, state=St} = State) ->
	%% close the outgoing connection
	iso_interface:line_down(OutConn),
	case catch Mod:response({Interface, Response}, Msg, St) of
		{request, {To, Request}, NewSt} ->
			iso_interface:send(To, Request, me()),
		    {next_state, wait_response,
		    State#state{out_req={To,Request}, state=NewSt},
		    State#state.timeout};
		{request, {To, Request}, NewSt, Timeout} ->
			iso_interface:send(To, Request, me()),
		    {next_state, wait_response,
		    State#state{out_req={To,Request}, state=NewSt},
		    Timeout};
		{advice, {To, Request}, Response, NewSt} ->
			iso_interface:send(From, Response),
			iso_interface:store_and_forward(To, Request),
		    {stop, normal, State#state{state=NewSt}};
		{response, Response, NewSt} ->
			iso_interface:send(From, Response),
		    {stop, normal, State#state{state=NewSt}};
		{stop, NewSt} ->
		    {stop, normal, State#state{state=NewSt}};
		{'EXIT', Reason} -> %% {'EXIT', Reason}
			exit(Reason)
	end;

%% Error received from the original interface
wait_response({From, {Error, _Msg}}, #state{from=From} = State)
	when Error =:= line_down orelse Error =:= timeout ->
	{stop, Error, State}.

%%====================================================================
%% Internal functions
%%====================================================================

me() ->
	Pid = self(),
	fun(X) -> ?MODULE:send(Pid, X) end.

