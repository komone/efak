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

%% @doc	This module is the primary interface to interact with the iso8583 interfaces configured in the efak cluster.
%%
%%		Incoming messages are delivered to your application via the callback function specified in the configuration file for each configured iso8583 interface.
%%		The callback function can be changed programmatically by your application via the `set_callback' function.
%%
%%		Your application can send messages to the iso8583 interfaces via `send' and `store_and_forward' functions.
%%		The destination interface is selected according the `dest()' type when using `send' and `store_and_forward'.
%% @end
%%
%% @type	isomsg()	= list(Field)
%%						Field	= {Id, Value}
%%						Id		= integer().
%%						ISO8583 message is modeled as a list of tuples where each tuple represent an iso fileld. `Id' is the field id while `Value' is the value of the field as specified in the interface configuration.
%%
%% @type	dest()	= atom() | {atom(), pid()}.
%%					`atom()' is the name of the interface, while `{atom(),pid()}' is usually used to send responses to previously delivered messages via the same connection process `pid()'.
%%
%% @type 	callback() = CallbackTuple | CallbackFun
%% 					CallbackTuple 	= {M::atom(),F::atom(),A::list()}
%%					CallbackFun 	= function(InMsg)
%%					InMsg			= {From::dest(), isomsg()} | {From::dest(), Error}
%%					Error			= {ErrorType, OutMsg::isomsg()}
%%					ErrorType		= down | line_down | timeout.
%%					The interface uses `apply(M,F,A ++ InMsg)' to deliver incoming messages when `CallbackTuple' is provided, otherwise messages are delivered using `CallbackFun(InMsg)'. When `Error' occurs after sending `OutMsg', the interface will notify the `ErrorType' providing the original message `OutMsg'.
%%

-module(iso_interface).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_cluster/1, up/1, down/1, send/3, send/2, line_down/1,
		store_and_forward/2, store_and_forward/3, set_callback/2,
		signon/1, signoff/1, echotest/1]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("interface_conf.hrl").
-include("internal_msg.hrl").

-define(INTERVAL_TIMEOUT, 5000).	%% cut_off is checked every 5 seconds

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {conf, timer}).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Conf) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Conf	= #interface_conf{}
%%				Pid 	= pid()
%%
%% @doc		Starts the server as a gen_server_cluster.
%% @end	
%%--------------------------------------------------------------------
start_cluster(Conf) when is_record(Conf, interface_conf) ->
    gen_server_cluster:start(Conf#interface_conf.name, ?MODULE, [Conf], []).

%%--------------------------------------------------------------------
%% @spec	(Name) -> ok
%%
%%				Name	= atom()
%%
%% @doc		Force interface named Name in `up' status.
%%			Messages can flow from and to the interface.
%% @end	
%%--------------------------------------------------------------------
up(Name) ->
	gen_server:cast({global, Name}, up).

%%--------------------------------------------------------------------
%% @spec	(Name) -> ok
%%
%%				Name	= atom()
%%
%% @doc		Force interface named Name in `down' status.
%%			Only network class messages are allowed to or from the interface.
%% @end	
%%--------------------------------------------------------------------
down(Name) ->
	gen_server:cast({global, Name}, down).

%%--------------------------------------------------------------------
%% @spec	(Name) -> ok
%%
%%				Name		= dest()
%%
%% @doc		Sends a signon message to the remote interface.
%% @end
%%--------------------------------------------------------------------
signon(Name) when is_atom(Name) ->
	gen_server:cast({global, Name}, signon).

%%--------------------------------------------------------------------
%% @spec	(Name) -> ok
%%
%%				Name		= dest()
%%
%% @doc		Sends a signoff message to the remote interface.
%% @end
%%--------------------------------------------------------------------
signoff(Name) when is_atom(Name) ->
	gen_server:cast({global, Name}, signoff).

%%--------------------------------------------------------------------
%% @spec	(Name) -> ok
%%
%%				Name		= dest()
%%
%% @doc		Sends an echotest message to the remote interface.
%% @end
%%--------------------------------------------------------------------
echotest(Name) when is_atom(Name) ->
	gen_server:cast({global, Name}, echotest).

%%--------------------------------------------------------------------
%% @spec	(Name, Msg, Callback) -> ok
%%
%%				Name		= dest()
%%				Msg			= isomsg()
%%				Callback 	= callback()
%%
%% @doc		Sends the message Msg to the interface.
%%			The interface is informed to deliver any further message
%% 			via the Callback function.
%%
%%			The provided Callback function substitutes the default 
%% 			callback of the interface for all further messages
%% 			coming from the established connection.
%% @end
%%--------------------------------------------------------------------
send(Name, Msg, {Mod, Fun, Arg} = Callback)
	when is_atom(Name), is_list(Msg), is_atom(Mod), is_atom(Fun), is_list(Arg) ->
	ok = iso_handler:send(#imsg{cmd=data, src=Callback, dst={Name, undefined}, msg=Msg});

send(Name, Msg, Fun)
	when is_atom(Name), is_list(Msg), is_function(Fun) ->
	ok = iso_handler:send(#imsg{cmd=data, src=Fun, dst={Name, undefined}, msg=Msg});

send({Name, Pid} = From, Msg, {Mod, Fun, Arg} = Callback)
	when is_atom(Name), is_pid(Pid), is_list(Msg), is_atom(Mod), is_atom(Fun), is_list(Arg) ->
	ok = iso_handler:send(#imsg{cmd=data, src=Callback, dst=From, msg=Msg});

send({Name, Pid} = From, Msg, Fun)
	when is_atom(Name), is_pid(Pid), is_list(Msg), is_function(Fun) ->
	ok = iso_handler:send(#imsg{cmd=data, src=Fun, dst=From, msg=Msg}).

%%--------------------------------------------------------------------
%% @spec	(Name, Msg) -> ok
%%
%%				Name		= dest()
%%				Msg			= isomsg()
%%
%% @doc		Sends the message Msg to the interface.
%%			May be used when sending a response to a previous request and
%%			no further messages are expected.
%% @end
%%--------------------------------------------------------------------
send(Name, Msg)	when is_atom(Name), is_list(Msg) ->
	ok = iso_handler:send(#imsg{cmd=data, src=undefined, dst={Name, undefined}, msg=Msg});

send({Name, Pid} = From, Msg) when is_atom(Name), is_pid(Pid), is_list(Msg) ->
	ok = iso_handler:send(#imsg{cmd=data, src=undefined, dst=From, msg=Msg}).

%%--------------------------------------------------------------------
%% @spec	(From) -> ok
%%
%%				From		= dest()
%%
%% @doc		Instructs the specified connection to close.
%%			This function is used by the application to close a
%% 			connection previuosly provided via the callabck function.
%%
%%			Typical scenario is closing the connection after
%% 			receiving a response message.
%% @end
%%--------------------------------------------------------------------
line_down({Name, Pid} = From) when is_atom(Name), is_pid(Pid) ->
	ok = iso_handler:send(#imsg{cmd=line_down, dst=From}).

%%--------------------------------------------------------------------
%% @spec	(Interface, Msg) -> ok
%%
%%				Interface	= dest()
%%				Msg			= isomsg()
%%
%% @doc		Sends the message Msg asyncronously to the interface.
%%			Using this function no response will be sent back to the
%% 			application but messages are in any case logged.
%% @end
%%--------------------------------------------------------------------
store_and_forward(Interface, Msg) when is_list(Msg) ->
	store_and_forward(Interface, Msg, undefined);

store_and_forward({Interface,_}, Msg) when is_list(Msg) ->
	store_and_forward(Interface, Msg, undefined).

%%--------------------------------------------------------------------
%% @spec	(Name, Msg, Callback) -> ok
%%
%%				Name		= dest()
%%				Msg			= isomsg()
%%				Callback	= callback()
%%
%% @doc		Sends the message Msg asyncronously to the interface.
%%			The interface is informed to deliver any further message
%% 			via the Callback function.
%%
%%			The provided Callback function substitutes the default
%% 			callback of the interface for all further messages
%% 			coming from the established connection.
%% @end
%%--------------------------------------------------------------------
store_and_forward(Name, Msg, {Mod, Fun, Arg} = Callback)
	when is_atom(Name), is_list(Msg), is_atom(Mod), is_atom(Fun), is_list(Arg) ->
	ok = iso_saf:add(#imsg{cmd=data, src=Callback, dst={Name, undefined}, msg=Msg});

store_and_forward(Name, Msg, Callback)
	when is_atom(Name), is_list(Msg), is_function(Callback) ->
	ok = iso_saf:add(#imsg{cmd=data, src=Callback, dst={Name, undefined}, msg=Msg});

store_and_forward({Name, _}, Msg, {Mod, Fun, Arg} = Callback)
	when is_atom(Name), is_list(Msg), is_atom(Mod), is_atom(Fun), is_list(Arg) ->
	ok = iso_saf:add(#imsg{cmd=data, src=Callback, dst={Name, undefined}, msg=Msg});

store_and_forward({Name, _}, Msg, Callback)
	when is_atom(Name), is_list(Msg), is_function(Callback) ->
	ok = iso_saf:add(#imsg{cmd=data, src=Callback, dst={Name, undefined}, msg=Msg}).

%%--------------------------------------------------------------------
%% @spec	(Name, Callback) -> ok
%%
%%				Name		= dest()
%%				Callback	= callback()
%%
%% @doc		Sets the callback function for the specified interface.
%%			All incoming messages will be delivered to your
%% 			application via the new Callback function.
%% @end
%%--------------------------------------------------------------------
set_callback(Interface, {M,F,A} = Callback) when is_atom(M), is_atom(F), is_list(A) ->
	gen_server:call({global, Interface}, {set_callback, Callback});

set_callback(Interface, Fun) when is_function(Fun) ->
	gen_server:call({global, Interface}, {set_callback, Fun}).

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
init([Conf]) ->
	{Date,_} = calendar:local_time(),
	{ok, TRef} = timer:send_interval(?INTERVAL_TIMEOUT, check_cutoff),
	{ok, #state{
			conf	= Conf#interface_conf{current_date = calendar:date_to_gregorian_days(Date)},
			timer	= TRef}
	}.

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

%% @clear

%%--------------------------------------------------------------------
%% @spec	({set_callback, Callback}, From, State) -> {reply, Reply, State}
%%
%%				Callback 	= {atom(),atom(),list()} | function(term())
%%				Reply 		= ok | ko
%%
%% @doc		Sets the new callback function Callback for the interface.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({set_callback, Callback}, _From, State) ->
	Reply = iso_handler:set_callback(State#interface_conf.name, Callback),
	{reply, Reply, State#interface_conf{callback=Callback}};

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

%% @clear

%%--------------------------------------------------------------------
%% @spec	(NewStatus, State) -> {noreply, State}
%%
%%				NewStatus	= up | down
%%
%% @doc		Force interface into the status NewStatus.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(NewStatus, #state{conf=Conf} = State)
			when NewStatus =:= up orelse NewStatus =:= down ->
    error_logger:info_msg("Interface ~p is getting ~p~n",
   						[Conf#interface_conf.name, NewStatus]),
	iso_handler:change_status(Conf#interface_conf.name, NewStatus),
	{noreply, State#state{
					conf = Conf#interface_conf{status = NewStatus} }
	};

handle_cast(Network, #state{conf=Conf} = State)
			when Network =:= signon orelse Network =:= signoff
			orelse Network =:= echotest ->
	call_app(Conf#interface_conf.callback,
		{{Conf#interface_conf.name, undefined}, {Network, []}}),
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

%% @clear

%%--------------------------------------------------------------------
%% @spec	(check_cutoff, State) -> {noreply, State}
%%
%% @doc		Verifies if the reconciliation date has changed and
%% 			updates it if required.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(check_cutoff, #state{conf=#interface_conf{name=Name, current_date=CurrentDate, cut_off=CutOffTime}} = State) ->
	NextDate = calendar:gregorian_days_to_date(CurrentDate+1),
	NextCutOff = calendar:datetime_to_gregorian_seconds({NextDate, CutOffTime}),
	Now = calendar:local_time(),
	SecsNow = calendar:datetime_to_gregorian_seconds(Now),
	if
		SecsNow > NextCutOff ->
			{{Year, Month, Day},_} = Now,
			NewCurrentDate = calendar:date_to_gregorian_days(Year, Month, Day),
			iso_handler:change_date(Name, NewCurrentDate),
			#state{conf=Conf} = State,
			{noreply, State#state{
							conf = Conf#interface_conf{current_date = NewCurrentDate} }
			};
		true ->
			{noreply, State}
	end;

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
terminate(_Reason, #state{timer=TRef}) ->
	timer:cancel(TRef),
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
%% @spec	(Destination, Msg) -> ok
%%
%%				Destination		= undefined | {M,F,A} | function(term())
%%				Msg				= {Line, M}
%%
%% @doc		Sends the message Msg to the Destination process, if
%% 			Destination is known.
%%			If Destination is a {M,F,A} tuple the message is sent
%% 			via apply(M,F,A ++ Msg).
%%			If Destination is a fun/1 function the message is sent
%% 			via fun(Msg).
%% @hidden
%% @end
%%--------------------------------------------------------------------
call_app(undefined, _Msg) ->
	ok;

call_app({M,F,A}, Msg) ->
	ok = apply(M, F, A ++ [Msg]);

call_app(Fun, Msg) when is_function(Fun) ->
	ok = Fun(Msg).

