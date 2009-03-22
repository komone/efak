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

%% @doc	Sample transaction for handling 1100 messages.
%% 		Requests with amount greater than 9.99 euro gets declined.
%% @see bg_example
%% @end

-module(bg_auth_tx).

-behaviour(iso_transaction).

-export([should_start/1, init/0, request/2, response/3, timeout/3, terminate/2,
		command/2]).

%%--------------------------------------------------------------------
%% @spec	({Interface, Msg}) -> true | false
%%
%%				Interface	= atom()
%%				Msg			= isomsg() | {Command, Msg}
%%
%% @doc     Replies true if the transaction can process the message
%%			Msg coming from the interface named Interface.
%%
%%			This transaction is interested only in network (x8xx)
%% 			messages and interface commands.
%% @end
%%--------------------------------------------------------------------
should_start({_Interface, {call, {_From, {auth, _}}}}) ->
	%%report({?MODULE,should_start}),
	true;
should_start({_Interface, {cast, {auth, _}}}) ->
	%%report({?MODULE,should_start}),
	true;
should_start({_Interface, Msg}) when is_list(Msg) ->
	%%report({?MODULE,should_start}),
	%%report(Msg),
	case iso:get_MTI(Msg) of
		<<_,$1,_,_>> -> true;
		_ -> false
	end;
should_start(_) ->
	false.

%%--------------------------------------------------------------------
%% @spec	() -> {ok, State}
%%
%% @doc     Initialize the transaction state.
%% @end
%%--------------------------------------------------------------------
init() ->
	%%report({?MODULE,init}),
	{ok, {}}.

%%--------------------------------------------------------------------
%% @spec	({Interface, Request}, State) ->
%%						{request, {To, Request}, NewState}			|
%%						{request, {To, Request}, NewState, Timeout}	|
%%						{advice, {To, Request}, Response, NewState}	|
%%						{response, Response, NewState}				|
%%						{stop, NewState}
%%
%%				Interface	= atom()
%%				Request		= isomsg()
%%				To			= atom()
%%				Response	= isomsg()
%%
%% @doc     Receives the request message Request from interface named Interface.
%% @end
%%--------------------------------------------------------------------
request({_Interface, Request}, State) ->
	%%report({?MODULE,request}),
	{_, Response} = authorize(Request, auth_proc()),
	{response, Response, State}.

%%--------------------------------------------------------------------
%% @spec	({Interface, Response}, Request, State) ->
%%						{request, {To, Request}, NewState}			|
%%						{request, {To, Request}, NewState, Timeout}	|
%%						{advice, {To, Request}, Response, NewState}	|
%%						{response, Response, NewState}				|
%%						{stop, NewState}
%%
%%				Interface	= atom()
%%				Request		= isomsg()
%%				To			= atom()
%%				Response	= isomsg()
%%
%% @doc     Receives the response message Response from interface
%% 			named Interface. Request is the first message received
%%			by the transaction.
%% @end
%%--------------------------------------------------------------------
response({_Interface, Response}, {call, {From, {auth, _}}}, State) ->
	%%report({?MODULE,response}),
	erl_interface:reply(From, Response),
	{stop, State};

response({_Interface, _Response}, {cast, {auth, _}}, State) ->
	%%report({?MODULE,response}),
	{stop, State}.

%%--------------------------------------------------------------------
%% @spec	({Interface, LastRequest}, FirstRequest, State) ->
%%						{request, {To, Request}, NewState}			|
%%						{request, {To, Request}, NewState, Timeout}	|
%%						{advice, {To, Request}, Response, NewState}	|
%%						{response, Response, NewState}				|
%%						{stop, NewState}
%%
%%				Interface		= atom()
%%				LastRequest		= isomsg()
%%				FirstRequest	= isomsg()
%%
%% @doc     The transaction or the interface named Interface timed out
%% 			while waiting for the response to the LastRequest message.
%%			FirstRequest is the original request that started the transaction.
%% @end
%%--------------------------------------------------------------------
timeout({_Interface, _LastRequest}, {call, {From, {auth, _}}}, State) ->
	%%report({?MODULE,timeout}),
	erl_interface:reply(From, timeout),
	{stop, State};

timeout({_Interface, _LastRequest}, {cast, {auth, _}}, State) ->
	%%report({?MODULE,timeout}),
	{stop, State}.

%%--------------------------------------------------------------------
%% @spec	(Reason, State) -> ok
%%
%%				Reason = normal | line_down | timeout
%%
%% @doc     The transaction is going to stop.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	%%report({?MODULE,terminate,Reason}),
	ok.

%%--------------------------------------------------------------------
%% @spec	({Interface, Command}, State) ->
%%						{request, {To, Request}, NewState}	        |
%%						{request, {To, Request}, NewState, Timeout}	|
%%						{advice, {To, Request}, NewState}	        |
%%						{stop, NewState}
%%
%%				Interface	= atom()
%%				Command		= signon | signoff | echotest
%%				Request		= isomsg()
%%				To			= atom()
%%
%% @doc     The interface named Interface sends the Command.
%% @end
%%--------------------------------------------------------------------
command({_Interface, {call, {_From, {auth, Amount}}}}, State) ->
	%%report({?MODULE,call}),
	Request = prepare_1100(Amount),
	{request, {outgoing, Request}, State};

command({_Interface, {cast, {auth, Amount}}}, State) ->
	%%report({?MODULE,cast}),
	Request = prepare_1100(Amount),
	{request, {outgoing, Request}, State}.

prepare_1100(Amount) ->
	{Date,Time} = calendar:local_time(),
	RRN = lists:flatten(io_lib:format("~s~6..0w", [iso_util:to_list(Date),iso_util:stan()])),
	[
	{0, "1100"},
	{2, "4444333322221111"},
	{3, "000000"},
	{4, Amount},
	{7, lists:nthtail(2, iso_util:to_list(calendar:universal_time()))},
	{11, iso_util:stan()},
	{12, iso_util:to_list({Date,Time})},
	{22, "1000101U3130"},
	{24, 100},
	{26, 7333},
	{32, 65401000001},
	{37, RRN},
	{41, "88888888"},
	{42, "555555555555555"},
	{43, "BESTPICS SHOP\\\\CITY\\0100      XXXYYY"},
	{49, 978},
	{53, "SECURITYINFO"}
	].

bmp_3({ok, M} = Valid) ->
	case iso_util:to_number(iso:get_field(3, M)) of
		0 -> Valid;
		10000 -> Valid;
		90000 -> Valid;
		_ -> {ko, iso:set_field(39, 904, M)} %% FORMAT ERROR
	end.

bmp_22({ok ,M} = Valid) ->
	case iso:get_field(22, M) of
		<<P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12>>
		when	(P1 == $1 orelse P1 == $2 orelse P1 == $5 orelse P1 == $7)
		andalso (P2 == $0 orelse P2 == $1 orelse P2 == $U)
		andalso (P3 == $0 orelse P3 == $1)
		andalso (P4 == $0 orelse P4 == $1 orelse P4 == $2)
		andalso (P5 == $0 orelse P5 == $1)
		andalso (P6 == $0 orelse P6 == $1)
		andalso (P7 == $1 orelse P7 == $2 orelse P7 == $5 orelse P7 == $7 orelse P7 == $S)
		andalso (P8 == $1 orelse P8 == $5 orelse P8 == $U)
		andalso (P9 == $1 orelse P9 == $3 orelse P9 == $4)
		andalso (P10 == $1 orelse P10 == $3)
		andalso (P11 == $2 orelse P11 == $3 orelse P11 == $4)
		andalso (P12 == $0 orelse (P12 >= $4 andalso P12 =< $C)) ->
			Valid;
		_ -> {ko, iso:set_field(39, 904, M)} %% FORMAT ERROR
	end.

bmp_24({ok, M} = Valid) ->
	case iso_util:to_number(iso:get_field(24, M)) of
		100 -> Valid;
		101 -> Valid;
		_ -> {ko, iso:set_field(39, 904, M)} %% FORMAT ERROR
	end.

accept_or_decline({ok, M}) ->
	case iso_util:to_number(iso:get_field(4, M)) of
		DeclinedAmount when DeclinedAmount > 999 ->
			M1 = iso:set_field(39, 116, M), %% NOT SUFFICENT FUNDS
			OrigAmount = lists:flatten(io_lib:format("~12..0w~12..0w", [DeclinedAmount, 0])),
			M2 = iso:set_field(30, OrigAmount, M1),
			M3 = iso:set_field(4, 0, M2),
			{ko, M3};
		_ ->
			M1 = iso:set_field(39, 0, M), %% APPROVED
			M2 = iso:set_field(38, iso_util:approval_code(), M1),
			{ok, M2}
	end.

auth_proc() ->
	[
	fun bmp_3/1,
	fun bmp_22/1,
	fun bmp_24/1,
	fun accept_or_decline/1
	].

authorize(M, CheckList) when is_list(M), is_list(CheckList) ->
	authorize({ok, M}, CheckList);
authorize({ko, M}, _) ->
	{ko, iso_util:reply_echo(M)};
authorize({ok, M}, []) ->
	{ok, iso_util:reply_echo(M)};
authorize(Ok, [Check | Rest]) when is_tuple(Ok), is_function(Check) ->
	authorize(Check(Ok), Rest).

%%report([E | _ ] = Message) when is_tuple(E)->
%%	error_logger:info_report(lists:keysort(1, Message));

%%report(Term) ->
%%	error_logger:info_report(Term).

