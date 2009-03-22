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

%% @doc	Test module for the Berlin Group example application.
%%
%% 		The example application is based on the protocol specification 
%% 		available at [http://www.berlin-group.org/]. The application uses
%% 		the {@link transaction_mgr} and the {@link iso_transaction} behaviour.
%%		In the `issuer' and `acquirer' subdirectory of `ebin/bg_example' you can
%% 		find the efak configuration files required to implement the Berlin
%% 		Group protocol.
%%
%% 		<em>The example application is NOT an implementation of the Berlin Group
%% 		specification, it is a simple demonstration of how to use and 
%% 		configure efak.</em>
%%
%% 		== Application setup ==
%% 		After compiling efak, cd into the ebin/bg_example/issuer directory.
%% 		Type `./start_issuer' and at the erlang shell type `efak_ctl:init_node()'.
%% 		Start the sample application with `application:start(bg_example)'.
%% 		From another shell cd into ebin/bg_example/acquirer, type `./start_acquirer',
%%		`efak_ctl:init_node()' and finally start the application with
%%		`application:start(bg_example)'.
%%
%% 		Now you have two nodes, one accepting incoming requests (issuer) and the
%%		other ready to fire outgoing requests (acquirer). The issuer application
%% 		uses an iso_interface named `incoming' and the acquirer application
%% 		uses an iso_interface named `outgoing'.
%%
%% 		== Testing ==
%% 		From the acquirer node you can fire transactions from the erlang shell
%%		using the bg_example module.
%%
%% 		`bg_example:signon(outgoing)' starts a signon transaction.
%% 		`bg_example:signoff(outgoing)' starts a signoff transaction.
%% 		`bg_example:echotest(outgoing)' starts an echotest transaction.
%% 		`bg_example:auth(10)' starts an authorization transaction for 0.10 euro,
%% 		as result you will get the reply message delivered by the issuer application.
%% 		`bg_example:reverse(Auth)' starts a reversal transaction for a previously
%% 		approved authorization transaction, to use it save the result of the
%%		`bg_example:auth(10)' in the variable Auth.
%%
%%		You can look in the interface log of both the issuer and the acquirer
%% 		application using the {@link ilb} module.
%% @end

-module(bg_example).

-export([signon/1, signoff/1, echotest/1, auth/1, reverse/1]).
-export([repeat/1, do_repeat/2]).

signon(Interface) ->
	iso_interface:signon(Interface).

signoff(Interface) ->
	iso_interface:signoff(Interface).

echotest(Interface) ->
	iso_interface:echotest(Interface).

auth(Amount) when is_integer(Amount) ->
	erl_interface:call({auth, Amount}, infinity).

reverse(Auth) when is_list(Auth) ->
	erl_interface:call({rev, Auth}, infinity).

repeat(Times) when is_integer(Times) ->
	{Time, Result} = timer:tc(?MODULE, do_repeat, [Times, dict:new()]),
	io:format("~n *** Average TPS : ~w ***~n", [1000000 * Times / Time]),
	dict:to_list(Result).

%% @hidden
do_repeat(0, D) ->
	D;
do_repeat(Times, D) ->
	case auth(10) of
		Atom when is_atom(Atom) -> R = Atom;
		Msg when is_list(Msg) -> R = iso:get_field(39, Msg);
		Else -> R = Else
	end,
	do_repeat(Times - 1, dict:update_counter(R, 1, D)).

