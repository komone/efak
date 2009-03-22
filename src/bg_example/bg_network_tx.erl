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

%% @doc	Sample transaction for handling 1804 messages.
%% @see bg_example
%% @end

-module(bg_network_tx).

-behaviour(iso_transaction).

-export([should_start/1, init/0, request/2, response/3, timeout/3, terminate/2,
		command/2]).

%%
%% Transaction state
%%
-record(state, {name, retries = 3}).

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
should_start({_Interface, {Cmd, _}})
	when Cmd =:= signon orelse Cmd =:= signoff orelse Cmd =:= echotest ->
	%%report({?MODULE,should_start}),
	true;
should_start({_Interface, Msg}) when is_list(Msg) ->
	%%report({?MODULE,should_start}),
	%%report(Msg),
	case iso:get_MTI(Msg) of
		<<_,$8,_,_>> -> true;
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
	report({?MODULE,init}),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @spec	({Interface, Request}, State) ->
%%						{request, {To, Request}, NewState}			|
%%						{request, {To, Request}, NewState, Timeout} |
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
request({Interface, Request}, State) ->
	report({?MODULE,request}),
	case iso_util:to_number(iso:get_field(24, Request)) of
		801 ->
			R1 = iso_util:reply_echo(Request),
			R2 = iso:set_field(39, 800, R1),
			iso_interface:up(Interface),
			{response, R2, State};
		802 ->
			R1 = iso_util:reply_echo(Request),
			R2 = iso:set_field(39, 800, R1),
			iso_interface:down(Interface),
			{response, R2, State};
		831 ->
			R1 = iso_util:reply_echo(Request),
			R2 = iso:set_field(39, 800, R1),
			iso_interface:up(Interface),
			{response, R2, State}
	end.

%%--------------------------------------------------------------------
%% @spec	({Interface, Response}, Request, State) ->
%%						{request, {To, Request}, NewState}			|
%%						{request, {To, Request}, NewState, Timeout} |
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
response({Interface, Response}, Request, State) ->
	report({?MODULE,response,Request}),
	case iso_util:to_number(iso:get_field(39, Response)) of
		800 -> 
			%% The first request received is the interface
			%% command
			case Request of
				{signoff,_} -> iso_interface:down(Interface);
				_ 	-> iso_interface:up(Interface)
			end;
		_ -> ok
	end,
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
timeout({_Interface, _LastRequest}, _FirstRequest, #state{retries=0} = State) ->
	report({?MODULE,timeout}),
	%% no response received and no more retries available, let's
	%% stop the transaction
	{stop, State};

timeout({Interface, LastRequest}, _FirstRequest, #state{retries=N} = State) ->
	report({?MODULE,timeout}),
	%% no response received, let's send againg the network request
	%% and update the retries count
	{request, {Interface, LastRequest}, State#state{retries=N-1}}.

%%--------------------------------------------------------------------
%% @spec	(Reason, State) -> ok
%%
%%				Reason = normal | line_down | timeout
%%
%% @doc     The transaction is going to stop.
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #state{name=Name} = _State) ->
	report({?MODULE,terminate,Reason}),
	%% unregister to allow a new network transaction to be started
	%% for the interface
	global:unregister_name(Name),
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
command({Interface, {Command,_}}, State) ->
	report({?MODULE,interface_cmd}),
	%% the interface is asking for a network request
	%% if no other network request is already running
	%% we start a new one.
	%% to ensure this is the only network transaction running
	%% in the cluster global:registe_name/2 is used
	Name = list_to_atom(lists:flatten([atom_to_list(?MODULE), "_", atom_to_list(Interface)])),
	case global:register_name(Name, self()) of
		yes -> send_network_request({Interface, Command}, State#state{name=Name});
		no -> {stop, State}
	end.

send_network_request({Interface, Command}, State) ->
	case Command of
		signon ->
			R1 = prepare_1804(Interface),
			R2 = iso:set_field(24, 801, R1);
		signoff ->
			R1 = prepare_1804(Interface),
			R2 = iso:set_field(24, 802, R1);
		echotest ->
			R1 = prepare_1804(Interface),
			R2 = iso:set_field(24, 831, R1)
	end,
	{request, {Interface, R2}, State}.

prepare_1804(incoming) ->
	[{25, 8601} | prepare_1804()];
prepare_1804(outgoing) ->
	[{25, 8600} | prepare_1804()].

prepare_1804() ->
	[
	{0,"1804"},
	{11, iso_util:stan()},
	{12, iso_util:to_list(calendar:local_time())},
	{53, "SECURITYINFO"}
	%% fields 93 and 94 are added by the interface if omitted
	%% field 128 is always calculated by the interface, even
	%% if already present in the message
	].

report([E | _ ] = Message) when is_tuple(E)->
	error_logger:info_report(lists:keysort(1, Message));

report(Term) ->
	error_logger:info_report(Term).

