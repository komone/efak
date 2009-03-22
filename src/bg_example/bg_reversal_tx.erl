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

%% @doc	Sample transaction for handling 1420 messages.
%% @see bg_example
%% @end

-module(bg_reversal_tx).

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
should_start({_Interface, {call, {_From, {rev, _}}}}) ->
	%%report({?MODULE,should_start}),
	true;
should_start({_Interface, {cast, {rev, _}}}) ->
	%%report({?MODULE,should_start}),
	true;
should_start({_Interface, Msg}) when is_list(Msg) ->
	%%report({?MODULE,should_start}),
	%%report(Msg),
	case iso:get_MTI(Msg) of
		<<_,$4,_,_>> -> true;
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
response({_Interface, Response}, {call, {From, {rev, _}}}, State) ->
	report({?MODULE,response}),
	erl_interface:reply(From, Response),
	{stop, State};

response({_Interface, _Response}, {cast, {rev, _}}, State) ->
	report({?MODULE,response}),
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
timeout({_Interface, _LastRequest}, {call, {From, {rev, _}}}, State) ->
	report({?MODULE,timeout}),
	erl_interface:reply(From, timeout),
	{stop, State};

timeout({_Interface, _LastRequest}, {cast, {rev, _}}, State) ->
	report({?MODULE,timeout}),
	{stop, State}.

%%--------------------------------------------------------------------
%% @spec	(Reason, State) -> ok
%%
%%				Reason = normal | line_down | timeout
%%
%% @doc     The transaction is going to stop.
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
	report({?MODULE,terminate,Reason}),
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
command({_Interface, {call, {_From, {rev, Authorization}}}}, State) ->
	%%report({?MODULE,call}),
	Request = prepare_1420(Authorization),
	{request, {outgoing, Request}, State};

command({_Interface, {cast, {auth, Authorization}}}, State) ->
	%%report({?MODULE,cast}),
	Request = prepare_1420(Authorization),
	{request, {outgoing, Request}, State}.

prepare_1420(Auth) ->
	case iso_util:to_list(iso:get_MTI(Auth)) of
		[V,F,$1,O] -> MTI = [V,F,$0,O];
		[V,F,$3,O] -> MTI = [V,F,$2,O];
		Any -> MTI = Any
	end,
	Reversal = [
	{0, "1420"},
	{7, lists:nthtail(2, iso_util:to_list(calendar:universal_time()))},
	{11, iso_util:stan()},
	{24, 400},
	{25, 4000},
	{53, "SECURITYINFO"},
	{56, lists:flatten(io_lib:format("~4..0s~6..0s~12..0s~2..0w~s",
									[iso_util:to_list(MTI),
									iso_util:to_list(iso:get_field(11, Auth)),
									iso_util:to_list(iso:get_field(12, Auth)),
									size(iso:get_field(32, Auth)),
									iso_util:to_list(iso:get_field(32, Auth))]))},
	{64, "*MACMAC*"}
	],
	iso:merge(Reversal, Auth).

bmp_3({ok, M} = Valid) ->
	case iso_util:to_number(iso:get_field(3, M)) of
		0 -> Valid;
		10000 -> Valid;
		90000 -> Valid;
		_ -> {ko, iso:set_field(39, 904, M)} %% FORMAT ERROR
	end.

bmp_24({ok, M} = Valid) ->
	case iso_util:to_number(iso:get_field(24, M)) of
		400 -> Valid;
		401 -> Valid;
		_ -> {ko, iso:set_field(39, 904, M)} %% FORMAT ERROR
	end.

bmp_25({ok ,M} = Valid) ->
	case iso_util:to_number(iso:get_field(25, M)) of
		4000 -> Valid;
		4001 -> Valid;
		4002 -> Valid;
		4004 -> Valid;
		4007 -> Valid;
		4013 -> Valid;
		4014 -> Valid;
		4015 -> Valid;
		4017 -> Valid;
		4019 -> Valid;
		4021 -> Valid;
		_ -> {ko, iso:set_field(39, 904, M)} %% FORMAT ERROR
	end.

accept_or_decline({ok, M}) ->
	case iso_util:to_number(iso:get_field(4, M)) of
		DeclinedAmount when DeclinedAmount > 999 ->
			M1 = iso:set_field(39, 110, M), %% ORIGINAL AMOUNT INCORRECT
			{ko, M1};
		_ ->
			M1 = iso:set_field(39, 400, M), %% APPROVED
			{ok, M1}
	end.

auth_proc() ->
	[
	fun bmp_3/1,
	fun bmp_24/1,
	fun bmp_25/1,
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

report([E | _ ] = Message) when is_tuple(E)->
	error_logger:info_report(lists:keysort(1, Message));

report(Term) ->
	error_logger:info_report(Term).

