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

%%
%% @doc Common iso functions.
%% @end
%%

-module(iso_util).

-export([stan/0, msg_type/1, mti_type/1, reply_echo/1, approval_code/0,
		make_tx_key/2]).
-export([to_number/1, to_date/1, to_datetime/1, to_list/1]).

%%--------------------------------------------------------------------
%% @spec	() -> integer()
%%
%% @doc		Returns a random integer smaller than 1000000.
%% @end
%%--------------------------------------------------------------------
stan() -> erlang:phash2({make_ref(),now()},1000000).

%%--------------------------------------------------------------------
%% @spec	(Message) -> request | response | notification
%%
%%				Message	 = isomsg()
%%
%% @doc		Returns the type of the message according to the MTI.
%% @end
%%--------------------------------------------------------------------
msg_type(Msg) ->
	mti_type(iso:get_MTI(Msg)).

%%--------------------------------------------------------------------
%% @spec	(MTI) -> request | response | notification
%%
%%				MTI	 = binary() | list()
%%
%% @doc		Returns the type of the message according to the MTI.
%% @end
%%--------------------------------------------------------------------
mti_type(<<_,_,$0,_>>) -> request;
mti_type(<<_,_,$1,_>>) -> response;
mti_type(<<_,_,$2,_>>) -> request;
mti_type(<<_,_,$3,_>>) -> response;
mti_type([_,_,$0,_]) -> request;
mti_type([_,_,$1,_]) -> response;
mti_type([_,_,$2,_]) -> request;
mti_type([_,_,$3,_]) -> response;
mti_type(_) -> notification.

%%--------------------------------------------------------------------
%% @spec	(Request) -> Response
%%
%%				Request	 = isomsg()
%%				Response = isomsg()
%%
%% @doc		Builds the Response message copying all the Request fields.
%% @end
%%--------------------------------------------------------------------
reply_echo(Msg) ->
	reply_echo(iso:get_MTI(Msg), Msg).

%%
%% prepare the reply to message Msg setting the MTI and fields 33, 100
%%
reply_echo(MTI, Msg) when is_binary(MTI) ->
	reply_echo(binary_to_list(MTI), Msg);
reply_echo([V,C,F,$1], Msg) when F =:= $0; F =:= $2 ->
	reply_echo([V,C,F,$0], Msg);
reply_echo([V,C,F,O], Msg) when F =:= $0; F =:= $2 ->
	case F of
		$0 -> %% authorization
			iso:set_MTI([V,C,$1,O], Msg);
		$2 -> %% notification
			iso:set_MTI([V,C,$3,O], Msg)
	end;
reply_echo(_, _Msg) ->
	[].

%%--------------------------------------------------------------------
%% @spec	() -> string()
%%
%% @doc		Returns a numeric random string of max 6 digits.
%% @end
%%--------------------------------------------------------------------
approval_code() ->
	integer_to_list(stan()).

%%--------------------------------------------------------------------
%% @spec	(Interface, Msg) -> ok
%%
%%				Interface	= atom()
%%				Msg			= [Field]
%%				Field		= {int(), Value}
%%
%% @doc		Builds the message key used to identify the message.
%%			The key is expected to be unique within the interface, the format is
%%
%%			{Interface, MTI, DateTime, RRN}
%%
%%			Interface is the name of the interface
%%			MTI is the message type indicator of the message
%%			DateTime is the value of field 12 DATETIME TRANSACTION
%%			RRN is the value of field 37 RETRIEVAL REFERENCE NUMBER
%%
%%			RRN is not available for network messages, if not present it defaults to 0.
%% @hidden
%% @end
%%--------------------------------------------------------------------
make_tx_key(Interface, M) ->
	MTI = 
	case iso:get_MTI(M) of
		[V,C,F,$1] -> to_number([V,C,F,$0]);
		[V,C,F,$3] -> to_number([V,C,F,$2]);
		Any		   -> to_number(Any)
	end,
	DT = to_number(iso:get_field(12, M)),
	Rrn = 
	case iso:has_field(37, M) of
		true ->	to_list(iso:get_field(37, M));
		false -> 0
	end,
	{Interface, MTI, DT, Rrn}.

%%--------------------------------------------------------------------
%% @spec	(N) -> integer()
%%
%%				N = integer() | list() | binary()
%%
%% @doc		Converts N to an integer.
%% @end
%%--------------------------------------------------------------------
to_number(N) when is_integer(N) -> N;
to_number(N) when is_list(N) -> list_to_integer(N);
to_number(N) when is_binary(N) -> to_number(binary_to_list(N)).

%%--------------------------------------------------------------------
%% @spec	(D) -> date()
%%
%%				D = integer() | list() | binary() | date()
%%
%% @doc		Returns the date representing the value of D.
%%			D may be `[Y1,Y2,M1,M2,D1,D2]', `[Y1,Y2,Y3,Y4,M1,M2,D1,D2]',
%%			an integer representing the gregorian date or a binary.
%% @end
%%--------------------------------------------------------------------
to_date(Bin) when is_binary(Bin) ->
	to_date(binary_to_list(Bin));
to_date([Y1,Y2,M1,M2,D1,D2]) ->
	YY = to_number([Y1,Y2]),
	Year =
	if
		YY < 50 -> 2000 + YY;
		true -> 1900 + YY
	end,
	{Year,to_number([M1,M2]),to_number([D1,D2])};
to_date([Y1,Y2,Y3,Y4,M1,M2,D1,D2]) ->
	{to_number([Y1,Y2,Y3,Y4]),to_number([M1,M2]),to_number([D1,D2])};
to_date(I) when is_integer(I) ->
	calendar:gregorian_days_to_date(I);
to_date({_Year,_Month,_Day} = Date) ->
	Date.

%%--------------------------------------------------------------------
%% @spec	(DT) -> datetime()
%%
%%				D = list() | binary() | datetime()
%%
%% @doc		Returns the datetime representing the value of DT.
%%			D may be `[Y1,Y2,M1,M2,D1,D2,H1,H2,N1,N2,S1,S2]'
%%			or a binary.
%% @end
%%--------------------------------------------------------------------
to_datetime(Bin) when is_binary(Bin) ->
	to_datetime(binary_to_list(Bin));
to_datetime([Y1,Y2,M1,M2,D1,D2,H1,H2,N1,N2,S1,S2]) ->
	{to_date([Y1,Y2,M1,M2,D1,D2]),
	 {to_number([H1,H2]),to_number([N1,N2]),to_number([S1,S2])}};
to_datetime({Date,Time} = DateTime) when is_tuple(Date), is_tuple(Time) ->
	DateTime.

%%--------------------------------------------------------------------
%% @spec	(X) -> string()
%%
%%				X = date() | time() | datetime() | list() | binary()
%%
%% @doc		Converts N to a string.
%% @end
%%--------------------------------------------------------------------
to_list(L) when is_list(L) -> L;
to_list({{Y,Mo,D},{H,Mi,S}}) ->
	lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0w",
								[Y rem 100,Mo,D,H,Mi,S]));
to_list({A,B,C}) ->
	lists:flatten(io_lib:format("~2..0w~2..0w~2..0w", [A rem 100,B,C]));
to_list(Bin) when is_binary(Bin) ->
	binary_to_list(Bin);
to_list(I) when is_integer(I) ->
	integer_to_list(I).


