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

%% @doc	Logs transactions handled by the efak node.
%%
%%		<em>Messages are stored without encryption and without
%% 		dropping any field, this strategy is not safe and is not
%% 		compliant with the security requirements needed for use in
%% 		a production environment.</em>
%% @end

-module(iso_log).

%%
%% @type	isomsg()	= list(Field)
%%						Field	= {Id, Value}
%%						Id		= integer().
%% ISO8583 message is modeled as a list of tuples where each tuple
%% represent an iso fileld. `Id' is the field id while `Value' is
%% the value of the field as specified in the interface configuration.
%%

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([log/2, log_timeout/3, check_duplicated/1,
		make_msg_key/2,	create_table/1, replicate_table/0]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("internal_msg.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-include("txlog.hrl").

-record(txtemp, { msg_key, ts, msg, cb }).

%%--------------------------------------------------------------------
%% @spec	(CurrentDate, Msg) -> ok
%%
%% 				CurrentDate = int()
%%				Msg 		= #imsg{}
%%
%% @doc		Logs the internal message Msg.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
log(CurrentDate, Msg) when is_record(Msg, imsg) ->
	inproc_log(CurrentDate, Msg).

%%--------------------------------------------------------------------
%% @spec	(CurrentDate, Interface, Msg) -> ok
%%
%%				Msg = #imsg{}
%%
%% @doc		Logs the timeout for the internal message Msg.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
log_timeout(CurrentDate, Interface, Msg) ->
	inproc_log_timeout(CurrentDate, Interface, Msg).

%%--------------------------------------------------------------------
%% @spec	(IMsg) -> ok | Orig
%%
%%				IMsg	= #imsg{}
%%				Orig	= [{int(), Value}]
%%
%% @doc		Checks if the internal message IMsg has already been
%% 			logged, the test applies only for new incoming requests.
%%			The message is searched in the log via the key
%%
%%			{Interface, MTI, DateTime, RRN}
%%
%%			Interface is the name of the interface
%%			MTI is the message type indicator of the message
%%			DateTime is the value of field 12 DATETIME TRANSACTION
%%			RRN is the value of field 37 RETRIEVAL REFERENCE NUMBER
%%
%%			RRN is not available for network messages, if not present
%% 			it defaults to 0.
%%
%%			If no records are found ok is returned else the found
%% 			message is returned.
%% @hidden
%% @end	
%%--------------------------------------------------------------------
check_duplicated(#imsg{cmd=data, src={Interface,_},dst=undefined,msg=M}) ->
	Result =
	case mnesia:dirty_read({txlog, iso_util:make_tx_key(Interface,M)}) of
		[] ->
			ok;
		[#txlog{type=data,tx=Tx}] ->
			Tx;
		[#txlog{type=_,tx=_}] ->
			ok;
		Many when length(Many) > 1 ->
			%% complex case: we have duplicated transactions. the one we are
			%% going to pick up is the oldest completed transaction.
			NewRes = lists:filter(fun
									(#txlog{type=data}) -> true;
									(_) 				-> false
								end, lists:keysort(3, Many)),
			case NewRes of
				[#txlog{type=data,tx=Tx2} | _] ->
					Tx2;
				_ ->
					ok
			end
	end,
	Result;
check_duplicated(Msg) when is_record(Msg, imsg) ->
	ok.

%%--------------------------------------------------------------------
%% @spec	(Interface, Msg) -> ok
%%
%%				Interface	= atom()
%%				Msg			= [Field]
%%				Field		= {int(), Value}
%%
%% @doc		Builds the txtemp key used to identify the record in the
%% 			ram only table.
%%			The message is stored temporarly in the txtemp table,
%% 			waiting for the response to gets deleted. The key in the
%% 			table is {Interface, STAN}.
%% @hidden
%% @end
%%--------------------------------------------------------------------
make_msg_key(Interface, M) ->
	Stan = iso_util:to_number(iso:get_field(11, M)),
	{Interface, Stan}.

%%--------------------------------------------------------------------
%% @spec	(Nodes) -> {atomic, ok} | {aborted, Reason}
%%
%%				Nodes	= [node()]
%%
%% @doc		Creates the log tables on node Nodes.
%%			The tables are txtemp and txlog, txtemp is of replica
%% 			type ram_copies, txlog is of replica type disc_only_copies.
%% 			The different replica type is justified because txlog can
%% 			be huge while txtemp should contain only temporary messages
%% 			that gets deleted by the logging procedure itself.
%%
%%			The txtemp table is intended only for internal use and must
%% 			not be accessed by other processes while txlog is available
%% 			for the application.
%%
%%			Logging takes place when the transaction request and response
%% 			are available, requests waiting for responses are stored in
%% 			the in the ram table txtemp.
%%			The transaction record is composed of only one iso message
%% 			containing both request and response fields, with precedence
%% 			to response fields.
%% @end
%%--------------------------------------------------------------------
create_table(Nodes) ->
	{atomic, ok} = mnesia:create_table(txtemp, [{ram_copies, Nodes},
										{attributes, record_info(fields, txtemp)}]),
	%% txlog is of type bag because we have to log duplicated transactions
	mnesia:create_table(txlog,	[{disc_only_copies, Nodes}, {type, bag},
						{attributes, record_info(fields, txlog)}]).

%%--------------------------------------------------------------------
%% @spec	() -> {atomic, ok} | {aborted, Reason}
%%
%% @doc		Replicates the log tables on the current node.
%% @end
%%--------------------------------------------------------------------
replicate_table() ->
	{atomic, ok} = mnesia:add_table_copy(txtemp, node(), ram_copies),
	mnesia:add_table_copy(txlog, node(), disc_only_copies).

%%--------------------------------------------------------------------
%% @spec	(CurrentDate, Msg) -> Callback
%%
%% 				CurrentDate = int()
%%				Msg = #imsg{}
%%
%% @doc		Logs the internal message Msg.
%%			Tests logging within the calling process using sync
%%			transaction to avoid delay in logging.
%%			Having one process per node to do logging seems not a 
%%			good idea since the messages stay queued for too much
%%			time.
%% @hidden
%% @end
%%--------------------------------------------------------------------
inproc_log(CurrentDate, #imsg{cmd=Type,src={Interface,_},dst=Dst,msg=M}) ->
	{atomic, Result} = mnesia:sync_transaction(
		fun() ->
			do_log(CurrentDate, Interface, Type, M, Dst)
		end
	),
	Result;
inproc_log(CurrentDate, #imsg{cmd=Type,src=Src,dst={Interface,_},msg=M}) ->
	{atomic, Result} = mnesia:sync_transaction(
		fun() ->
			do_log(CurrentDate, Interface, Type, M, Src)
		end
	),
	Result.

%%--------------------------------------------------------------------
%% @spec	(CurrentDate, Interface, Msg) -> {ok, Callback} | {ko, nil}
%%
%% 				CurrentDate = int()
%% 				Interface 	= atom()
%%				Msg 		= isomsg()
%%
%% @doc		Tries to log a timout for the message Msg.
%% @hidden
%% @end
%%--------------------------------------------------------------------
inproc_log_timeout(CurrentDate, Interface, Msg) ->
	{atomic, Result} = mnesia:sync_transaction(
		fun() ->
			do_log_timeout(CurrentDate, Interface, Msg)
		end
	),
	Result.

%%--------------------------------------------------------------------
%% @spec	(BusinessDay, Interface, Type, M, Callback) -> ok
%%
%% @doc		Logs the message into the txlog table.
%%			Logging takes place when the transaction request and
%% 			response are available, requests waiting for responses
%% 			are stored in the in the ram table txtemp.
%%			The transaction record is composed of only one iso message
%% 			containing both request and response fields, with
%% 			precedence to response fields.
%% @see iso:merge/2
%% @hidden
%% @end
%%--------------------------------------------------------------------
do_log(BusinessDay, Interface, Type, M, Callback) ->
	MsgKey = make_msg_key(Interface, M),
	case mnesia:read({txtemp, MsgKey}) of
		[] ->
			mnesia:write(#txtemp{
							msg_key = MsgKey,
							ts = now(),
							msg = M,
							cb = Callback
						}),
			{ok, Callback};
		[#txtemp{ts=T1, msg=Req, cb=Callback0}] ->
			mnesia:delete({txtemp, MsgKey}),
			%% delete the bitmap field
			%%Tx0 = lists:keydelete(1,1,iso:merge(M, Req)),
			Tx0 = iso:merge(M, Req),
			Tx = iso:set_MTI(iso:get_MTI(Req), Tx0),
			T2 = now(),
			case iso:has_field(28, Tx) of
				true ->
					{Year,Month,Day} = iso_util:to_date(iso:get_field(28, Tx)),
					CurrentDate = calendar:date_to_gregorian_days(Year,Month,Day);
				false ->
					CurrentDate = BusinessDay
			end,
			mnesia:write(#txlog{
							tx_key = iso_util:make_tx_key(Interface, Tx),
							ts = T2,
							elapsed = timer:now_diff(T2, T1) div 1000,
							interface = Interface,
							bday = CurrentDate,
							type = Type,
							tx = Tx
						}),
			{ok, Callback0}
	end.

%%--------------------------------------------------------------------
%% @spec	(CurrentDate, Interface, Msg) -> {ok, Callback} | {ko, nil}
%%
%% @doc		Logs the timeout message if it is still pending.
%% @hidden
%% @end
%%--------------------------------------------------------------------
do_log_timeout(BusinessDay, Interface, Msg) ->
	MsgKey = make_msg_key(Interface, Msg),
	case mnesia:read({txtemp, MsgKey}) of
		[] ->
			%% an event has already been received
			%% no need to handle the timeout
			{ko, nil};
		[#txtemp{ts=T1, cb=Callback0, msg=M}] ->
			mnesia:delete({txtemp, MsgKey}),
			T2 = now(),
			case iso:has_field(28, Msg) of
				true ->
					{Year,Month,Day} = iso_util:to_date(iso:get_field(28, Msg)),
					CurrentDate = calendar:date_to_gregorian_days(Year,Month,Day);
				false ->
					CurrentDate = BusinessDay
			end,
			mnesia:write(#txlog{
							tx_key = iso_util:make_tx_key(Interface, Msg),
							ts = T2,
							elapsed = timer:now_diff(T2, T1) div 1000,
							interface = Interface,
							bday = CurrentDate,
							type = timeout,
							tx = M
						}),
			{ok, Callback0}
	end.

