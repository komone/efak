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

%% @doc	Stores and forwards advice messages.
%%
%%		<em>Messages are stored without encryption and without dropping any field, this strategy is not safe and is not compliant with the security requirements needed for use in a production environment.</em>
%% @end

-module(iso_saf).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1, recv/2, add/1, create_table/1, stats/0, stats/1,
		send/1, send/2, replicate_table/0]).

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

-define(FORCE_MAX_MSGS, 20).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(saf, {saf_key, interface, bday, retry, next_timeout, msg}).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(#interface_conf{}) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Pid = pid()
%%
%% @doc		Starts the server as a local server.
%%			The registered name is INTERFACENAME_saf.
%% @end	
%%--------------------------------------------------------------------
start_link(Conf) when is_record(Conf, interface_conf) ->
	Name = list_to_atom(lists:flatten([atom_to_list(Conf#interface_conf.name), "_saf"])),
	gen_server:start_link({local, Name}, ?MODULE, Conf, []).

%%--------------------------------------------------------------------
%% @spec	(Name, {From, Msg}) -> ok
%%
%%				Name	= atom()
%%				From	= {Name, pid()}
%%				Name	= atom()
%%				Msg		= [Field] | {Error, [Field]}
%%				Field	= {int(), Value}
%%
%% @doc		Receives response to a previous saf request.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
recv(Name, {_From, _Msg} = Response) when is_atom(Name) ->
	Interface = list_to_atom(lists:flatten([atom_to_list(Name), "_saf"])),
	gen_server:cast(Interface, {recv, Response}).

%%--------------------------------------------------------------------
%% @spec	(Msg) -> ok
%%
%%				Msg		= #imsg{}
%%
%% @doc		Adds the message Msg to the saf.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
add(#imsg{cmd=data, dst={Name, undefined}} = IMsg) ->
	Interface = list_to_atom(lists:flatten([atom_to_list(Name), "_saf"])),
	gen_server:cast(Interface, {add, IMsg}).

%%--------------------------------------------------------------------
%% @spec	(Nodes) -> {atomic, ok} | {aborted, Reason}
%%
%%				Nodes	= [node()]
%%
%% @doc		Creates the saf table on node Nodes.
%%			Saf is of replica type disc_copies.
%%
%%			Messages are recorded per interface, the key is expected to be unique within the interface.
%%
%% 			The key is {Interface, MTI, DateTime, RRN}
%%
%%			Interface is the name of the interface
%%			MTI is the message type indicator of the message
%%			DateTime is the value of field 12 DATETIME TRANSACTION
%%			RRN is the value of field 37 RETRIEVAL REFERENCE NUMBER
%%
%%			RRN is not available for network messages, 0 is used.
%%
%%			Note: suggestions are welcome to identify a better unique key that can work for all iso versions.
%% @hidden
%% @end
%%--------------------------------------------------------------------
create_table(Nodes) ->
	mnesia:create_table(saf, [{disc_copies, Nodes},
						{attributes, record_info(fields, saf)}]).

%%--------------------------------------------------------------------
%% @spec	() -> {atomic, ok} | {aborted, Reason}
%%
%% @doc		Replicates the saf table on the current node.
%% @end
%%--------------------------------------------------------------------
replicate_table() ->
	mnesia:add_table_copy(saf, node(), disc_copies).

%%--------------------------------------------------------------------
%% @spec	() -> Status::list()
%%
%% @doc		Returns the saf status.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
stats() ->
	status().

%%--------------------------------------------------------------------
%% @spec	(Name) -> Status::list()
%%
%%				Name = atom()
%%
%% @doc		Returns the saf status for interface named Name.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
stats(Name) when is_atom(Name) ->
	status(Name).

%%--------------------------------------------------------------------
%% @spec	(Name) -> ok
%%
%%				Name = atom()
%%
%% @doc		Sends expired messages still present in the saf for the
%%			specfied interface Name.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
send(Name) when is_atom(Name) ->
	send(Name, ?FORCE_MAX_MSGS).

%%--------------------------------------------------------------------
%% @spec	(Name, Max) -> ok
%%
%%				Name 	= atom()
%%				Max 	= integer()
%%
%% @doc		Sends at most Max expired messages still present in the
%% 			saf for the	specfied interface Name.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
send(Name, Max) when is_atom(Name), is_integer(Max) ->
	Interface = list_to_atom(lists:flatten([atom_to_list(Name), "_saf"])),
	gen_server:cast(Interface, {send_expired, Max}).

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
%% @doc     Initiates the server.
%%			The saf register the interface within the line_proxy servers
%%			because the iso_interface init function is not called by
%%			gen_server_cluster when started as a replicated server.
%%			This strategy allows the registration of the interface on
%%			all line_proxy nodes with a running interface.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(Conf) ->
	ok = line_proxy:register_interface(Conf#interface_conf.name),
	{ok, Conf, msecs_to_first(Conf#interface_conf.name)}.

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
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State, msecs_to_first(State#interface_conf.name)}.

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
%% @spec	({recv, {From, Msg}}, State) ->	{noreply, State}
%%
%%				From	= {Name, pid()}
%%				Name	= atom()
%%				Msg		= [Field]
%%				Field	= {int(), Value}
%%
%% @doc		Checks if a response have been received for a previous saf request.
%%
%%			In case a delivery error has been received (timeout, line_down or
%%			interface down) no actions are taken so the saf will try to send
%% 			more repeats.
%%
%%			In case of a valid response from the remote interface, the field
%%			39 (ACTION CODE) is tested before removing the transaction from the
%% 			saf. For the following codes the transaction is not removed
%%
%%			907 ISSUER OR SWITCH INOPERATIVE
%%			909 SYSTEM MALFUNCTION
%%			910 CARD ISSUER SIGNED OFF
%%			911 TIMEOUT OF ISSUER RESPONSE
%%			912 CARD ISSER UNAVAILABLE
%%
%%			For others codes it is assumed a valid (though not accepted request)
%% 			response has been received so the transaction gets deleted from the
%%			saf. The transaction is present in the interface log.
%%
%%			If delivery errors occurs the transaction may not gets logged but
%%			it will still be present in the saf.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({recv, {_From, {_Error, Msg}}},
			#interface_conf{name=Name, callback=C} = State) ->
	%% if the retries are expired an echo message is sent
	Key = iso_util:make_tx_key(Name, Msg),
	{atomic, #saf{retry=Retry}} =
	mnesia:transaction(fun() ->	[Rec] = mnesia:read(saf, Key, read), Rec end),
	case Retry of
		0 -> call_app(C, {{Name, undefined}, {echotest, []}});
		_ -> ok
	end,
	{noreply, State, msecs_to_first(State#interface_conf.name)};

handle_cast({recv, {From, Msg}}, #interface_conf{name=Name, callback=C} = State)
			when is_list(Msg) ->
	case iso_util:to_number(iso:get_field(39, Msg)) of
		%% in case of the following codes an echo-test 
		%% is sent to the remote interface
		907 -> call_app(C, {{Name, undefined}, {echotest, []}});
		909 -> call_app(C, {{Name, undefined}, {echotest, []}});
		910 -> call_app(C, {{Name, undefined}, {echotest, []}});
		911 -> call_app(C, {{Name, undefined}, {echotest, []}});
		912 -> call_app(C, {{Name, undefined}, {echotest, []}});
		_ ->
			Callback = remove(State#interface_conf.name, Msg),
			call_app(Callback, {From, Msg})
	end,
	{noreply, State, msecs_to_first(State#interface_conf.name)};

%%--------------------------------------------------------------------
%% @spec	({add, Msg}, State) ->	{noreply, State}
%%
%%				Msg		= #imsg{}
%%
%% @doc		Adds the message Msg to the saf.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({add, Msg}, State) ->
	add(Msg, State#interface_conf.repeat_timeout, State#interface_conf.retry),
	{noreply, State, msecs_to_first(State#interface_conf.name)};

handle_cast({send_expired, Max}, State) ->
	send_expired(State#interface_conf.name, Max),
	{noreply, State, msecs_to_first(State#interface_conf.name)};

handle_cast(_Msg, State) ->
	{noreply, State, msecs_to_first(State#interface_conf.name)}.

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
%% @spec	(timeout, State) -> {noreply, State}
%%
%% @doc		Time to send saf messages.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
	send_from_saf(State#interface_conf.name, State#interface_conf.repeat_timeout),
	{noreply, State, msecs_to_first(State#interface_conf.name)};

handle_info(_Info, State) ->
	{noreply, State, msecs_to_first(State#interface_conf.name)}.

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

%%--------------------------------------------------------------------
%% @spec	(Name) -> none | int()
%%
%% @doc		Count milliseconds to wait for the oldest saf message that needs to be sent.
%% @hidden
%% @end
%%--------------------------------------------------------------------
msecs_to_first(Name) ->
	Trans = fun() ->
		MatchHead = #saf{
						interface=Name,
						next_timeout='$1',
						retry='$2',
						_='_' },
		Guards = [{'>', '$2', 0}],
		Result = [['$1']],
		mnesia:select(saf, [{MatchHead,Guards,Result}])
	end,
	{atomic, Results} = mnesia:transaction(Trans),
	Now = now_in_secs(),
	FirstTimeout = lists:foldl(fun
								([Timeout], Acc) when Timeout < Acc ->	Timeout;
								(_, Acc) -> Acc
								end, Now + 60, Results),
	if
		Now >= FirstTimeout -> 500;
		true -> (FirstTimeout - Now) * 1000
	end.

%%--------------------------------------------------------------------
%% @spec	(Msg, Timeout, MaxRetry) -> ok
%%
%%				Msg			= #imsg{}
%%				Timeout		= int()
%%				MaxRetry 	= int()
%%
%% @doc		Adds the message Msg to the saf table, setting the number of maximum retries and the next point in time required to resend the message if response is unavailable.
%%
%%			The message is sent to the interface just after insertion in the saf table.
%% @hidden
%% @end
%%--------------------------------------------------------------------
add(#imsg{dst={Name,_}, msg=M} = Msg, Timeout, MaxRetry) ->
	Trans = fun() ->
				mnesia:write(#saf{
								saf_key=iso_util:make_tx_key(Name,M),
								interface=Name,
								%% bday=BusinessDay,
								retry=MaxRetry,
								next_timeout=now_in_secs() + round(Timeout / 1000),
								msg=Msg})
	end,
	{atomic, ok} = mnesia:transaction(Trans),
	iso_handler:send(Msg#imsg{src=fun(X) -> ?MODULE:recv(Name, X) end}).

%%--------------------------------------------------------------------
%% @spec	(Interface, Timeout) -> ok
%%
%%				Interface	= atom()
%%				Timeout		= int()
%%
%% @doc		Sends all messages with retries available to the interface.
%% @hidden
%% @end
%%--------------------------------------------------------------------
send_from_saf(Name, Timeout) ->
	MatchHead = #saf{
					saf_key='$1',
					interface=Name,
					retry='$2',
					next_timeout='$3',
					msg='$4',
					_='_'},
	Guard = [{'>','$2',0},{'<','$3', now_in_secs()}],
	Result = [['$1','$4']],
	Trans =	fun() ->
		Results = mnesia:select(saf, [{MatchHead,Guard,Result}]),
		lists:map(fun([Key,Msg]) ->
					[Rec] = mnesia:read(saf,Key,write),
					NewRetry = case Rec#saf.retry of
						0 -> 0;
						I -> I - 1
					end,
					ok = mnesia:write(Rec#saf{
										retry=NewRetry,
										next_timeout=now_in_secs() + round(Timeout / 1000)}),
					Msg
		end, Results)
	end,
	{atomic, MsgList} = mnesia:transaction(Trans),
	report_saf_activity(Name, MsgList),
	lists:foreach(fun(#imsg{msg=M} = Msg) ->
					M1=iso:set_field(11, iso_util:stan(), M),
					M2 =
					case iso:get_MTI(M1) of
						[V,C,F,$0]		-> iso:set_MTI([V,C,F,$1],M1);
						<<V,C,F,$0>>	-> iso:set_MTI([V,C,F,$1],M1);
						[V,C,F,$2]		-> iso:set_MTI([V,C,F,$3],M1);
						<<V,C,F,$2>>	-> iso:set_MTI([V,C,F,$3],M1);
						_Any			-> M1
					end,
					iso_handler:send(Msg#imsg{
										src=fun(X) -> ?MODULE:recv(Name, X) end,
										msg=M2})
	end, MsgList).

%%--------------------------------------------------------------------
%% @spec	(Interface, Max) -> ok
%%
%%				Interface	= atom()
%%				Max 		= integer()
%%
%% @doc		Force sending at most Max messages with no more reties available.
%% @hidden
%% @end
%%--------------------------------------------------------------------
send_expired(Name, Max) ->
	MatchHead = #saf{
					interface=Name,
					retry=0,
					msg='$1',
					_='_'},
	Guard = [],
	Result = ['$1'],
	Trans =	fun() ->
		Results = mnesia:select(saf, [{MatchHead,Guard,Result}], Max, read),
		case Results of
			'$end_of_table' -> [];
			{Msgs, _} -> Msgs;
			_ -> []
		end
	end,
	{atomic, MsgList} = mnesia:transaction(Trans),
	report_saf_activity(Name, MsgList),
	lists:foreach(fun(#imsg{msg=M} = Msg) ->
					M1=iso:set_field(11, iso_util:stan(), M),
					M2 =
					case iso:get_MTI(M1) of
						[V,C,F,$0]		-> iso:set_MTI([V,C,F,$1],M1);
						<<V,C,F,$0>>	-> iso:set_MTI([V,C,F,$1],M1);
						[V,C,F,$2]		-> iso:set_MTI([V,C,F,$3],M1);
						<<V,C,F,$2>>	-> iso:set_MTI([V,C,F,$3],M1);
						_Any			-> M1
					end,
					iso_handler:send(Msg#imsg{
										src=fun(X) -> ?MODULE:recv(Name, X) end,
										msg=M2})
	end, MsgList).

%%
%% Returns this point in time expressed as seconds
%%
now_in_secs() ->
	calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%%--------------------------------------------------------------------
%% @spec	(Name, Msg) -> Callback
%%
%%				Name		= atom()
%%				Msg			= [Field]
%%				Field		= {int(), Value}
%%				Callback	= undefined | {M,F,A} | function(term())
%%
%% @doc		Remove the original request for response Msg from saf.
%%			Returns the callback in case the application wants to be informed.
%% @hidden
%% @end
%%--------------------------------------------------------------------
remove(Name, Msg) ->
	Orig = 
	case iso:get_MTI(Msg) of
		[V,C,$1,O] 	 -> iso:set_MTI([V,C,$0,O], Msg);	%% probably never used
		<<V,C,$1,O>> -> iso:set_MTI([V,C,$0,O], Msg);	%% probably never used
		[V,C,$3,O] 	 -> iso:set_MTI([V,C,$2,O], Msg);
		<<V,C,$3,O>> -> iso:set_MTI([V,C,$2,O], Msg)
	end,
	Key = iso_util:make_tx_key(Name,Orig),
	{atomic, #imsg{src=Callback}} =
	mnesia:transaction(fun() ->
							[Rec] = mnesia:read(saf, Key, sticky_write),
							mnesia:delete(saf, Key, sticky_write),
							Rec#saf.msg end),
	Callback.

%%--------------------------------------------------------------------
%% @spec	(Destination, Msg) -> ok
%%
%%				Destination		= undefined | {M,F,A} | function(term())
%%				Msg				= {Line, M}
%%
%% @doc		Sends the message Msg to the Destination process, if Destination is known.
%%			If Destination is a {M,F,A} tuple the message is sent via apply(M,F,A ++ Msg).
%%			If Destination is a fun/1 function the message is sent via fun(Msg).
%% @hidden
%% @end
%%--------------------------------------------------------------------
call_app(undefined, _Msg) ->
	ok;

call_app({M,F,A}, Msg) ->
	ok = apply(M, F, A ++ [Msg]);

call_app(Fun, Msg) when is_function(Fun) ->
	ok = Fun(Msg).

%%--------------------------------------------------------------------
%% @spec	() -> [{Key,Value}]
%%
%% @doc		Returns status of the saf table.
%% @hidden
%% @end
%%--------------------------------------------------------------------
status() ->
	MatchHead = #saf{
					interface='$1',
					next_timeout='$2',
					retry='$3',
					_='_'},
	Guards = [],
	Result = ['$1', '$2', '$3'],
	status({MatchHead, Guards, [Result]}).

%%--------------------------------------------------------------------
%% @spec	(Interface) -> [{Key,Value}]
%%
%%				Interface = atom()
%%
%% @doc		Returns status of the saf table for the specified Interface.
%% @hidden
%% @end
%%--------------------------------------------------------------------
status(Name) when is_atom(Name) ->
	MatchHead = #saf{
					interface=Name,
					next_timeout='$1',
					retry='$2',
					_='_'},
	Guards = [],
	Result = [Name, '$1', '$2'],
	status({MatchHead, Guards, [Result]});

status(MatchSpec) when is_tuple(MatchSpec) ->
	Trans = fun() ->
		mnesia:select(saf, [MatchSpec])
	end,
	{atomic, Results} = mnesia:transaction(Trans),
	Size = length(Results),
	case Size of
		0 -> [];
		_ -> 
			{MinR,MaxR,IList,Exp} = lists:foldl(fun([I,T,R], {Min,Max,IL,Ex}) ->
												Found = lists:member(I,IL),
												{
												if T < Min -> T; true -> Min end,
												if T > Max -> T; true -> Max end,
												if Found -> IL; true -> [I | IL] end,
												if R == 0 -> Ex + 1; true -> Ex end
												}
												end, {now_in_secs()+3600,0,[],0}, Results),
			[
			{interfaces, IList},
			{size, Size},
			{expired_retries, Exp},
			{oldest_retry, calendar:universal_time_to_local_time(
									calendar:gregorian_seconds_to_datetime(MinR))},
			{newest_retry, calendar:universal_time_to_local_time(
									calendar:gregorian_seconds_to_datetime(MaxR))}
			]
	end.

%%--------------------------------------------------------------------
%% @spec	(Interface, Msgs) -> ok
%%
%%				Interface 	= atom()
%%				Msgs		= [#imsg{}]
%%
%% @doc		Reports saf activity.
%% @hidden
%% @end
%%--------------------------------------------------------------------
report_saf_activity(Name, MsgList) ->
	case length(MsgList) of
		0 -> ok;
		1 -> 
			Report = lists:flatten(io_lib:format("Interface ~w is sending ~w saf repeat~n",
												[Name, 1])),
			error_logger:info_report(Report);
		Size -> 
			Report = lists:flatten(io_lib:format("Interface ~w is sending ~w saf repeats~n",
												[Name, Size])),
			error_logger:info_report(Report)
	end.

