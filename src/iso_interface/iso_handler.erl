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

%% @doc	Parses, builds and log iso8583 messages.
%%
%%			The interface verifies in the following order:
%%			<dl>
%%			<dt>that the message can be parsed correctly</dt>
%%				<dd>if not, the connection gets closed</dd>
%%			<dt>that the message is composed according to the message specification</dt>
%%				<dd>if not, a response is sent back with Action Code "FORMAT ERROR" (904)</dd>
%%			<dt>that the message mac, if present, is correct</dt>
%%				<dd>if not, a response is sent back with Action Code "INCORRECT MAC" (916)</dd>
%%			<dt>that the message is not a duplicate message and is not a repeat</dt>
%%				<dd>if it is duplicated and a repeat, the original response is send back</dd>
%%				<dd>if it is duplicated but not a repeat, a response is sent back with Action Code "DUPLICATE TRANSMISSION" (913)</dd>
%%			<dt>that the reconciliation date of the message, if present, is compatible with the current interface date</dt>
%%				<dd>if not, a response is sent back with Action Code "RECONCILIATION CUTOVER OR CHECKPIONT ERROR" (915)</dd>
%%			</dl>
%%		Messages are logged via the {@link iso_log} module.
%% @end

-module(iso_handler).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1, recv/2, send/1, timeout/2, change_status/2,
 		change_date/2, set_callback/2, set_timeout/2]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("internal_msg.hrl").
-include("interface_conf.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Conf) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Conf = #interface_conf{}
%%
%% @doc		Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Conf) when is_record(Conf, interface_conf) ->
	gen_server:start_link(?MODULE, Conf, []).

%%--------------------------------------------------------------------
%% @spec	(From, Bin) -> ok
%%
%%				From 		= {Interface, pid()}
%% 				Interface 	= atom()
%% 				Bin			= binary()
%%
%% @doc		Used by lines to deliver incoming messages.
%%			throws {unavailableHandler, Interface, Reason}
%% @hidden
%% @end
%%--------------------------------------------------------------------
recv({Name,Pid0} = From, Bin) when is_pid(Pid0), is_binary(Bin) ->
	case pg2:get_closest_pid(Name) of
		{error, Reason} ->
			throw({unavailableHandler, Name, Reason});
		Pid ->
			ok = gen_server:cast(Pid, {recv, From, Bin})
	end.

%%--------------------------------------------------------------------
%% @spec	(Msg) -> ok
%%
%%				Msg = #internal_msg{}
%%
%% @doc		Sends the message Msg to the interface.
%% @hidden
%% @throws {unavailableHandler, Interface, Reason}
%% @end
%%--------------------------------------------------------------------
send(#imsg{dst={Name,_}} = Msg) ->
	%% error_logger:info_report([?MODULE,send,Msg]),
	case pg2:get_closest_pid(Name) of
		{error, Reason} ->
			throw({unavailableHandler, Name, Reason});
		Pid ->
			ok = gen_server:cast(Pid, {send, Msg})
	end.

%%--------------------------------------------------------------------
%% @spec	(Interface, Msg) -> ok
%%
%% 				Interface = atom()
%%				Msg = isomsg()
%%
%% @doc		Sends the timeout message to the interface.
%% @hidden
%% @end
%%--------------------------------------------------------------------
timeout(Interface, Msg) ->
	case pg2:get_closest_pid(Interface) of
		{error, _Reason} -> ok;
		Pid -> ok = gen_server:cast(Pid, {timeout, Msg})
	end.

%%--------------------------------------------------------------------
%% @spec	(Name, Status) -> ok
%%
%%				Name 	= atom()
%%				Status	= up | down
%%
%% @doc		Changes the status of all handlers to Status.
%% @hidden
%% @end
%%--------------------------------------------------------------------
change_status(Name, Status) when Status =:= up orelse Status =:= down ->
	lists:foreach(fun(Pid) -> gen_server:cast(Pid, {status, Status}) end,
					pg2:get_members(Name)),
	ok.

%%--------------------------------------------------------------------
%% @spec	(Name, NewDate) -> ok
%%
%%				Name 	= atom()
%%				NewDate	= int() "date in gregorian days"
%%
%% @doc		Changes the status of all handlers to Status.
%% @hidden
%% @end
%%--------------------------------------------------------------------
change_date(Name, NewDate) when is_integer(NewDate) ->
	lists:foreach(fun(Pid) -> gen_server:cast(Pid, {new_date, NewDate}) end,
					pg2:get_members(Name)),
	ok.

%%--------------------------------------------------------------------
%% @spec	(Name, NewCallback) -> ok | ko
%%
%%				Name 		= atom()
%%				NewCallback	= {atom(),atom(),list()} | function(term())
%%
%% @doc		Changes the status of all handlers to Status.
%% @hidden
%% @end
%%--------------------------------------------------------------------
set_callback(Name, NewCallback) ->
	Results =
	lists:map(fun(Pid) -> gen_server:call(Pid, {set_callback, NewCallback}) end,
					pg2:get_members(Name)),
	case lists:all(fun(Result) -> Result =:= ok end, Results) of
		true -> ok;
		false -> ko
	end.

%%--------------------------------------------------------------------
%% @spec	(Name, NewTimeout) -> ok | ko
%%
%%				Name 		= atom()
%%				NewTimeout	= integer()
%%
%% @doc		Changes the timeout of all handlers.
%% @hidden
%% @end
%%--------------------------------------------------------------------
set_timeout(Name, NewTimeout) ->
	Results =
	lists:map(fun(Pid) -> gen_server:call(Pid, {set_timeout, NewTimeout}) end,
					pg2:get_members(Name)),
	case lists:all(fun(Result) -> Result =:= ok end, Results) of
		true -> ok;
		false -> ko
	end.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Conf) -> {ok, State}
%%
%%				Conf = #interface_conf{}
%%
%% @doc     Initiates the server.
%%			Create and join the interface group and sets the current date.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(Conf) when is_record(Conf, interface_conf) ->
	pg2:create(Conf#interface_conf.name),
	pg2:join(Conf#interface_conf.name, self()),
	{Date,_} = calendar:local_time(),
    {ok, Conf#interface_conf{current_date=calendar:date_to_gregorian_days(Date)}}.

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
%% @spec	({set_callback, Callback}, From, State) -> ok
%%
%%				Callback	= {atom(),atom(),list()} | function(term())
%%
%% @doc		Sets a new callback for the interface.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({set_callback, Callback}, _From, State) ->
	{reply, ok, State#interface_conf{callback = Callback}};

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
%% @spec	({recv, Msg}, State) -> {noreply, State}
%%
%%				Msg		= #internal_msg{}
%%
%% @doc		Receives the message from the line, applies interface controls and sends the parsed message to the destination application via the callback funcion.
%%			When interface status is UP.
%%
%%			The interface verifies in the following order
%%			* that the message can be parsed correctly
%%				if not, the connection gets closed
%%			* that the message is composed according to the message specification
%%				if not, a response is sent back with Action Code "FORMAT ERROR" (904)
%%			* that the message mac, if present, is correct
%%				if not, a response is sent back with Action Code "INCORRECT MAC" (916)
%%			* that the message is not a duplicate message and is not a repeat
%%				if it is duplicated and a repeat, the original response is send back
%%				if it is duplicated but not a repeat, a response is sent back with Action Code "DUPLICATE TRANSMISSION" (913)
%%			* that the reconciliation date of the message, if present, is compatible with the current interface date
%%				if not, a response is sent back with Action Code "RECONCILIATION CUTOVER OR CHECKPIONT ERROR" (915)
%%
%%			Messages are logged via the iso_log module.
%%
%% @todo	Wirte checks on #interface_conf.local_inst_id and #interface_conf.remote_inst_id
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({recv, Line, Bin},
			#interface_conf{packer=P,status=up,security_module=SecMod} = I)
			when is_binary(Bin) ->
	try iso:unpack(P, Bin) of
		M -> 
			remove_timer(I#interface_conf.name, M),
			{ok, Callback} = iso_log:log(I#interface_conf.current_date,
										#imsg{cmd=data, src=Line, msg=M}),
			Msg = #imsg{cmd=data, src=Line, dst=Callback, msg=M},
			MTI = iso:get_MTI(M),
			{Mandatory, _} = fields_for_mti(MTI, I#interface_conf.validator),
			case validate_fields(M, Mandatory) of
				true ->
					case verify_mac(SecMod, Mandatory, M) of
						true ->
							case iso_log:check_duplicated(Msg) of
								ok ->
									case check_date(I#interface_conf.current_date,
													MTI, M) of
										true ->
											call_app(Callback,
													{Line,M},
													I#interface_conf.callback);
										false ->
											send_error_response(Msg, 915, I)
									end;
								Original ->
									reply_to_dup_tx(MTI, Msg, Original, I)
							end;
						false ->
							send_error_response(Msg, 916, I)
					end;
				false ->
					send_error_response(Msg, 904, I)
			end
	catch
		_:_ ->
			line_proxy:send(#imsg{cmd=line_down, dst=Line})
	end,
    {noreply, I};

%%--------------------------------------------------------------------
%% @spec	({recv, Msg}, State) -> {noreply, State}
%%
%%				Msg		= #internal_msg{}
%%
%% @doc		Receives the message from the line, applies interface controls and sends the responses back to the connection.
%%			When interface status is DOWN the application does not receive incoming messages.
%%
%%			The interface verifies in the following order
%%			* that the message can be parsed correctly
%%				if not, the connection gets closed
%%			* that the message is composed according to the message specification
%%				if not, a response is sent back with Action Code "FORMAT ERROR" (904)
%%			* that the message mac, if present, is correct
%%				if not, a response is sent back with Action Code "INCORRECT MAC" (916)
%%			* that the message is a network message
%%				if yes, the message is forwarded to the interface controller
%%				if not, a response is sent back with Action Code "CARD ISSUER SIGNED OFF" (910)
%%
%%			Messages are logged via the iso_log module.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({recv, Line, Bin},
			#interface_conf{packer=P,status=down, security_module=SecMod} = I)
			when is_binary(Bin) ->
	try iso:unpack(P, Bin) of
		M -> 
			remove_timer(I#interface_conf.name, M),
			{ok, Callback} = iso_log:log(I#interface_conf.current_date,
										#imsg{cmd=data, src=Line, msg=M}),
			Msg = #imsg{cmd=data, src=Line, dst=Callback, msg=M},
			MTI = iso:get_MTI(M),
			{Mandatory, _} = fields_for_mti(MTI,I#interface_conf.validator),
			case validate_fields(M, Mandatory) of
				true ->
					case verify_mac(SecMod, Mandatory, M) of
						true ->
							case MTI of
								<<_,$8,_,_>> ->
									call_app(Callback,
											{Line,M},
											I#interface_conf.callback);
								_Any ->
									send_error_response(Msg, 910, I)
							end;
						false ->
							send_error_response(Msg, 916, I)
					end;
				false ->
					send_error_response(Msg, 904, I)
			end
	catch
		_:_ ->
			line_proxy:send(#imsg{cmd=line_down, dst=Line})
	end,
    {noreply, I};

%%--------------------------------------------------------------------
%% @spec	({send, Msg}, State) -> {noreply, State}
%%
%%				Msg		= #internal_msg{}
%%
%% @doc		Forwards a message coming from the application when the
%% 			interface status is UP.
%%
%%			The interface verifies in the following order
%%			* that the message has been send to the correct interface
%%			* that the message is composed according to the message specification
%%				if not, the exception {internalMessageNotValid, Msg} is thrown
%%
%%			MTI is changed to reflect the version of the interface.
%%			Messages are logged via the iso_log module.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({send, #imsg{cmd=data, dst={N,_}, msg=M} = Msg},
			#interface_conf{name=N,iso_version=E, status=up, timeout=T,
							security_module=SecMod} = I) ->
	case iso:get_MTI(M) of
		[_,C,F,O] -> MTI = <<E,C,F,O>>;
		<<_,C,F,O>> -> MTI = <<E,C,F,O>>
	end,
	{Mandatory, Optional} = fields_for_mti(MTI, I#interface_conf.validator),
	M1 = add_mac(SecMod, Mandatory, set_inst_id(I, M)),
	case validate_fields(M1, Mandatory) of
		true ->
			send_to_line(Msg#imsg{msg=M1}, Mandatory, Optional, I),
			start_timer(N, T, M1);
		false ->
			notify_interface_error(internalMessageNotValid, Msg#imsg{msg=M1}, I#interface_conf.callback)
	end,
    {noreply, I};

%%--------------------------------------------------------------------
%% @spec	({send, Msg}, State) -> {noreply, State}
%%
%%				Msg		= #internal_msg{}
%%
%% @doc		Blocks a message coming from the application when the
%% 			interface status is DOWN. Only network messages are
%% 			forwarded to the line.
%%
%%			Messages are logged via the iso_log module.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({send, #imsg{cmd=data, dst={N,_}, msg=M} = Msg},
			#interface_conf{name=N, iso_version=E, status=down, timeout=T,
							security_module=SecMod} = I) ->
	case iso:get_MTI(M) of
		[_,C,F,O] -> MTI = <<E,C,F,O>>;
		<<_,C,F,O>> -> MTI = <<E,C,F,O>>
	end,
	case C of
		$8 ->
			{Mandatory, Optional} = fields_for_mti(MTI,
													I#interface_conf.validator),
			M1 = add_mac(SecMod, Mandatory, set_inst_id(I, M)),
			case validate_fields(M1,Mandatory) of
				true ->
					send_to_line(Msg#imsg{msg=M1}, Mandatory, Optional, I),
					start_timer(N, T, M1);
				false ->
					notify_interface_error(internalMessageNotValid, Msg#imsg{msg=M1}, I#interface_conf.callback)
			end;
		_ ->
			notify_interface_error(down, Msg#imsg{msg=M}, I#interface_conf.callback)
	end,
    {noreply, I};

%%--------------------------------------------------------------------
%% @spec	({send, Msg}, State) -> {noreply, State}
%%
%%				Msg		= #internal_msg{}
%%
%% @doc		Forwards errors/commands coming from the application to the line.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({send, #imsg{dst={N,_}} = Error}, #interface_conf{name=N} = I) ->
	line_proxy:send(Error),
    {noreply, I};

%%--------------------------------------------------------------------
%% @spec	({timeout, Msg}, State) -> {noreply, State}
%%
%%				Msg	= isomsg()
%%
%% @doc		Handles the timeout for message Msg.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({timeout, Msg}, #interface_conf{name=Interface, current_date=Date} = I) ->
	key_timers:remove(iso_log:make_msg_key(Interface, Msg)),
	case iso_log:log_timeout(Date, Interface, Msg) of
		{ok, Callback} -> 
			call_app(Callback,
					{{Interface,undefined}, {timeout, Msg}},
					I#interface_conf.callback);
		_ -> ok
	end,
    {noreply, I};

%%--------------------------------------------------------------------
%% @spec	({status, Status}, State) -> {noreply, State}
%%
%%				Status	= up | down
%%
%% @doc		Changes the status of the interface.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({status, Status}, I) when Status =:= up orelse Status =:= down ->
    {noreply, I#interface_conf{status=Status}};

%%--------------------------------------------------------------------
%% @spec	({new_date, NewDate}, State) -> {noreply, State}
%%
%%				NewDate	= int()
%%
%% @doc		Changes the current reconciliation date of the interface.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({new_date, NewDate}, I) when is_integer(NewDate) ->
    {noreply, I#interface_conf{current_date=NewDate}};

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
terminate(_Reason, State) ->
	pg2:leave(State#interface_conf.name, self()),
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
%% @spec	(MTI, Validator) -> {MandatoryFields, OptionalFields}
%%
%%				MTI				= string() | binary()
%%				Validator		= [{MTI, MandatoryFields, OptionalFields}]
%%				MandatoryFields	= [int()]
%%				OptionalFields 	= [int()]
%%
%% @doc		Returns the message specification for the given MTI
%% 			(Message Type Identifier).
%%			Repeat messages are treated as original messages.
%% @hidden
%% @end
%%--------------------------------------------------------------------
fields_for_mti(MTI,Validator) when is_binary(MTI) ->
	fields_for_mti(binary_to_list(MTI), Validator);
%% Acquirer repeat
fields_for_mti([Version,Class,Function,$1],Validator) ->
	fields_for_mti([Version,Class,Function,$0],Validator);
%% Issuer repeat
fields_for_mti([Version,Class,Function,$3],Validator) ->
	fields_for_mti([Version,Class,Function,$2],Validator);
%% Other repeat
fields_for_mti([Version,Class,Function,$5],Validator) ->
	fields_for_mti([Version,Class,Function,$4],Validator);
fields_for_mti(MTI,Validator) when is_list(MTI) ->
	{value, {_,M,O}} = lists:keysearch(MTI, 1, Validator),
	{M,O}.

%%--------------------------------------------------------------------
%% @spec	(Msg, FieldList) -> true | false
%%
%%				Msg				= [Field]
%%				Field			= {Id, Value}
%%				Id 				= int()
%%				FieldList 		= [Id]
%%
%% @doc		Checks if all fields in FieldList are present in Msg.
%% @hidden
%% @end
%%--------------------------------------------------------------------
validate_fields(Msg,FieldList) ->
	lists:all(fun(X) ->	lists:keymember(X,1,Msg) end, FieldList).

%%--------------------------------------------------------------------
%% @spec	(GregorianDate, MTI, Msg) -> true | false
%%
%%				GregorianDate	= int()
%%				MTI		 		= string() | binary()
%%				Msg				= [Field]
%%				Field			= {Id, Value}
%%				Id 				= int()
%%
%% @doc		Checks if field 28 Reconciliation Date of message Msg
%% 			is compatible with the GregorianDate.
%%			If field 28 is not present true is returned.
%%
%% @todo	Not sure if Field 28 is the Reconciliation Date in all
%% 			ISO version, probably this check should be in the 
%% 			application domain or a configurable parameter should
%% 			be passed in order to identify the correct field.
%% @hidden
%% @end
%%--------------------------------------------------------------------
check_date(Date, MTI, M) ->
	CurrentDate = Date,
	MsgDate =
	case iso:has_field(28, M) of
		true ->
			{Year,Month,Day} = iso_util:to_date(iso:get_field(28, M)),
			calendar:date_to_gregorian_days(Year,Month,Day);
		false ->
			CurrentDate
	end,
	check_date_internal(MTI, CurrentDate, MsgDate).

%%--------------------------------------------------------------------
%% @spec	(MTI, CurrentDate, MsgDate) -> true | false
%%
%%				MTI		 		= binary()
%%				CurrentDate		= int()
%%				MsgDate			= int()
%%
%% @doc		Checks if MsgDate (expressed in grgorian days) of message
%% 			type MTI is compatible with the CurrentDate.
%% @hidden
%% @end
%%--------------------------------------------------------------------
%% same date, return ok
check_date_internal(_,CurrentDate,CurrentDate) ->
	true;
%% it is allowed to accept messages with tomorrow reconciliation date
check_date_internal(_,CurrentDate,MsgDate) when MsgDate =:= CurrentDate + 1->
	true;
%% in case of notification messages, reconciliation date may be yesterday
check_date_internal(<<_,_,$2,_>>,CurrentDate,MsgDate) when MsgDate =:= CurrentDate - 1 ->
	true;
%% accept any date on response messages, don't care
check_date_internal(<<_,_,C,_>>,_,_) when C =:= $1 orelse C =:= $3 ->
	true;
%% other cases are not acceptable
check_date_internal(_,_,_) ->
	false.

%%--------------------------------------------------------------------
%% @spec	(Msg, ActionCode, Interface) -> ok
%%
%%				Msg			= #imsg{}
%%				ActionCode	= int()
%%				Interface 	= #interface_conf{}
%%
%% @doc		Sends an error response back to the connection if Msg is
%% 			a request message otherwise the connection is asked to close.
%%			The response is built according to the message specification 
%% 			for the interface and it gets logged with module iso_log.
%% @hidden
%% @end
%%--------------------------------------------------------------------
send_error_response(#imsg{msg=M, src=Line, dst=Dst} = Msg, Error,
					#interface_conf{packer=P, validator=V,
					security_module=SecMod, current_date=Date}) ->
	case iso_util:reply_echo(M) of
		[] ->
			line_proxy:send(#imsg{cmd=line_down, src=Dst, dst=Line});
		Reply ->
			{ManR, OptR} = fields_for_mti(iso:get_MTI(Reply),V),
			Reply0 = add_mac(SecMod, ManR, iso:set_field(39, Error, Reply)),
			PackMsg = pack_and_log(P, Msg#imsg{msg=Reply0}, ManR, OptR, Date),
			line_proxy:send(Msg#imsg{msg=PackMsg, src=Dst, dst=Line})
	end.

%%--------------------------------------------------------------------
%% @spec	(MTI, Msg, Original, InterfaceConf) -> ok
%%
%%				MTI			= binary()
%%				Msg			= #imsg{}
%%				Original	= [Field]
%%				Field		= {Id, Value}
%%				Id 			= int()
%%				InterfaceConf = #interface_conf{}
%%
%% @doc		Sends a response back to the connection in case of
%% 			duplicated message.	Reponse is logged with module iso_log.
%% @hidden
%% @end
%%--------------------------------------------------------------------
%% in case of repeat message : reply original message
reply_to_dup_tx(<<_,_,_,O>>, #imsg{msg=M, src=Line, dst=Dst} = Msg,	Original,
				#interface_conf{packer=Packer, iso_version=Ver,
				security_module=SecMod, current_date=Date})
	when O =:= $1 orelse O =:= $3 ->
	case iso_util:reply_echo(M) of
		[] ->
			line_proxy:send(#imsg{cmd=line_down, src=Dst, dst=Line});
		Reply ->
			{ManR, OptR} = fields_for_mti(iso:get_MTI(Reply),Ver),
			Reply0 = add_mac(SecMod, ManR, iso:merge(Reply, Original)), %% copy fields from original reply
			PackMsg = pack_and_log(Packer, Msg#imsg{msg=Reply0}, ManR, OptR, Date),
			line_proxy:send(Msg#imsg{msg=PackMsg, src=Dst, dst=Line})
	end;
%% if not a repeat message : reply DUPLICATE TRANSMISSION (913)
reply_to_dup_tx(_, #imsg{msg=M, src=Line, dst=Dst} = Msg, _, 
				#interface_conf{packer=Packer, iso_version=Ver,
				security_module=SecMod, current_date=Date}) ->
	case iso_util:reply_echo(M) of
		[] ->
			line_proxy:send(#imsg{cmd=line_down, src=Dst, dst=Line});
		Reply ->
			{ManR, OptR} = fields_for_mti(iso:get_MTI(Reply),Ver),
			Reply0 = add_mac(SecMod, ManR, iso:set_field(39, 913, Reply)),
			PackMsg = pack_and_log(Packer, Msg#imsg{msg=Reply0}, ManR, OptR, Date),
			line_proxy:send(Msg#imsg{msg=PackMsg, src=Dst, dst=Line})
	end.

%%--------------------------------------------------------------------
%% @spec	(Interface, Msg) -> ok
%%
%%				Interface 	= #interface_conf{}
%%				Msg			= [{integer(), Value}]
%%
%% @doc		Sets institution id for local and remote interfaces.
%% @hidden
%% @end
%%--------------------------------------------------------------------
set_inst_id(#interface_conf{iso_version=$0, local_inst_id=Lid, remote_inst_id=Rid}, M) ->
	M1 =
	case iso:has_field(33, M) of
		false ->
			iso:set_field(33, Lid, M);
		_ -> M
	end,
	case iso:has_field(100, M1) of
		false ->
			iso:set_field(100, Rid, M1);
		_ -> M1
	end;

set_inst_id(#interface_conf{local_inst_id=Lid, remote_inst_id=Rid}, M) ->
	M1 =
	case iso:has_field(33, M) of
		false ->
			iso:set_field(33, Lid, M);
		_ -> M
	end,
	M2 =
	case iso:has_field(100, M1) of
		false ->
			iso:set_field(100, Rid, M1);
		_ -> M1
	end,
	M3 =
	case iso:has_field(93, M2) of
		false ->
			iso:set_field(93, Rid, M2);
		_ -> M2
	end,
	case iso:has_field(94, M3) of
		false ->
			iso:set_field(94, Lid, M3);
		_ -> M3
	end.

%%--------------------------------------------------------------------
%% @spec	(Msg, Mandatory, Optional, Interface) -> ok
%%
%%				Msg			= #imsg{}
%%				Mandatory	= [int()]
%%				Optional 	= [int()]
%%				Interface 	= #interface_conf{}
%%
%% @doc		Sends a message coming from the application to the connection.
%%			Message is logged with module iso_log.
%% @hidden
%% @end
%%--------------------------------------------------------------------
send_to_line(Msg, Mandatory, Optional,
			#interface_conf{packer=P, current_date=Date}) ->
	PackMsg = pack_and_log(P, Msg, Mandatory, Optional, Date),
	line_proxy:send(Msg#imsg{msg=PackMsg}).

%%--------------------------------------------------------------------
%% @spec	(Error, Msg, Callback) -> ok
%%
%%				Error		= internalMessageNotValid | down
%%				Msg			= #imsg{}
%%				Callback	= undefined | {atom(),atom(),term()} | function(term())
%%
%% @doc		Notifies to the application the error Error.
%%			If the Error is useful for the application it is sent back
%% 			via the callback function otherwise an exception is thrown.
%% @hidden
%% @end
%%--------------------------------------------------------------------
notify_interface_error(internalMessageNotValid, Msg, _) when is_record(Msg, imsg) ->
	throw({internalMessageNotValid, Msg});

notify_interface_error(ErrorType, #imsg{src=Src, dst=Interface, msg=Msg}, Callback) ->
	call_app(Src, {Interface, {ErrorType, Msg}}, Callback).

%%--------------------------------------------------------------------
%% @spec	(Packer, Msg, Mandatory, Optional, CurrentDate) -> binary()
%%
%%				Packer		= isopackager
%%				Msg			= #imsg{}
%%				Mandatory	= [int()]
%%				Optional 	= [int()]
%% 				CurrentDate = int()
%%
%% @doc		Build the message Msg according to the specification and
%% 			logs it with iso_log module.
%% @hidden
%% @end
%%--------------------------------------------------------------------
pack_and_log(Packer, #imsg{msg=M} = Msg, Mandatory, Optional, CurrentDate) ->
	AllFields = lists:append(Mandatory,Optional),
	M1 = lists:filter(fun({Id,_}) ->
						lists:member(Id,AllFields) end,	M),
	iso_log:log(CurrentDate, Msg#imsg{msg=M1}),
	iso:pack(Packer, M1).

%%--------------------------------------------------------------------
%% @spec	(Destination, Msg, DefaultCallback) -> ok
%%
%%				Destination		= undefined | {M,F,A} | function(term())
%%				Msg				= {Line, M}
%%				DefaultCallback	= {M,F,A}
%%
%% @doc		Sends the message Msg to the Destination process, if 
%% 			Destination is undefined the DefaultCallback is called.
%%			If Destination is a {M,F,A} tuple the message is sent via
%% 			apply(M,F,A ++ Msg).
%%			If Destination is a fun/1 function the message is sent
%% 			via fun(Msg).
%% @hidden
%% @end
%%--------------------------------------------------------------------
call_app(undefined, Msg, {Mod, Fun, Arg}) ->
	ok = apply(Mod, Fun, Arg ++ [Msg]);
call_app({M,F,A}, Msg, _) ->
	ok = apply(M, F, A ++ [Msg]);
call_app(Fun, Msg, _) when is_function(Fun) ->
	ok = Fun(Msg).

%%--------------------------------------------------------------------
%% @spec	(Module, Fields, Msg) -> true | false
%%
%%				Fields	= [integer()]
%%				Msg	= [{integer(), Value}]
%%
%% @doc		Verifies if the mac (field 64 or 128) is equal to the
%%			result of function Module:calc_mac/1.
%% @hidden
%% @end
%%--------------------------------------------------------------------
verify_mac(Module, Fields, Msg) ->
	case lists:filter(fun
						(64) -> true;
						(128) -> true;
						(_) -> false end, Fields) of
		[] -> true;
		[MacFld] -> iso:get_field(MacFld, Msg) =:= Module:calc_mac(Msg)
	end.

%%--------------------------------------------------------------------
%% @spec	(Module, Fields, Msg) -> NewMsg
%%
%%				Fields	= [integer()]
%%				Msg		= [{integer(), Value}]
%%
%% @doc		Calculates the mac for the message Msg and adds it to the
%%			message Msg.
%% @hidden
%% @end
%%--------------------------------------------------------------------
add_mac(Module, Fields, Msg) ->
	case lists:filter(fun
						(64) -> true;
						(128) -> true;
						(_) -> false end, Fields) of
		[] -> Msg;
		[MacFld] -> iso:set_field(MacFld, Module:calc_mac(Msg), Msg)
	end.

%%--------------------------------------------------------------------
%% @spec	(Interface, Timeout, Msg) -> ok | ko
%%
%%				Interface	= atom()
%%				Timeout		= integer()
%% 				Msg 		= isomsg()
%%
%% @doc		Starts the timeout timer for the outgoing request messages.
%% @hidden
%% @end
%%--------------------------------------------------------------------
start_timer(Interface, Timeout, Msg) ->
	case iso_util:msg_type(Msg) of
		request ->
			key_timers:add(iso_log:make_msg_key(Interface, Msg),
							timer:apply_after(Timeout,
												?MODULE,
												timeout,
												[Interface, Msg]));
		_ -> ko
	end.

%%--------------------------------------------------------------------
%% @spec	(Interface, Msg) -> ok | ko
%%
%%				Interface	= atom()
%% 				Msg 		= isomsg()
%%
%% @doc		Removes the timeout timer when the response messages Msg
%% 			is received.
%% @hidden
%% @end
%%--------------------------------------------------------------------
remove_timer(Interface, Msg) ->
	case iso_util:msg_type(Msg) of
		response ->
			key_timers:remove(iso_log:make_msg_key(Interface, Msg));
		_ -> ko
	end.

