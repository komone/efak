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

-module(ts_iso8583).

-include("ts_profile.hrl").
-include("ts_iso8583.hrl").

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/1,
         session_defaults/0,
         parse/2,
         parse_config/2,
         new_session/0]).

-export([yymmdd/1,
		hhmmss/1,
		yymmddhhmmss/1,
		mmddhhmmssUTC/1,
		unique_6/1,
		unique_12/1]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session
%% Returns: {ok, ack_type = parse|no_ack|local, persistent = true|false} 
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true}.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
	#iso8583{}.

%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request ,
%% Args:	record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#iso8583_request{packager=P, size_header=SH, fields=F}) ->
	Bin = iso:pack(P, F),
	SizeBin =
	case SH of
		{big_endian, Size} ->
			Len = size(Bin),
			<<Len:Size/big>>;
		{little_endian, Size} ->
			Len = size(Bin),
			<<Len:Size/little>>;
		{display, Size} ->
			Len = size(Bin),
			string:right(integer_to_list(Len), Size, $0);
		nil -> <<>>
	end,
	list_to_binary([SizeBin, Bin]).

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:	Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
    {State#state_rcv{ack_done = true, datasize=0}, [], true};

parse(Data, State=#state_rcv{acc = [], request=Request,
							session=#iso8583{msg_len=0}}) ->
	#iso8583_request{size_header = SizeH} = Request#ts_request.param,
	case parse_len(SizeH, Data) of
		{0, Bin} ->
			{State#state_rcv{acc=Bin, datasize=size(Bin)},[],false};
		{Len, Bin} ->
			#state_rcv{session=Session} = State,
			parse(Bin, State#state_rcv{datasize=size(Bin),
										session=Session#iso8583{msg_len=Len}})
	end;

parse(Data, State=#state_rcv{acc = [], datasize=Size, request=Request,
							session=#iso8583{msg_len=Size}})
							when Size > 0 ->
	#iso8583_request{packager = Packager} = Request#ts_request.param,
	Fields = iso:unpack(Packager, Data),
    ts_mon:add({ count, iso8583_parsed_replies }),
    ts_mon:add({ count, iso:get_field(39, Fields) }),
	{State#state_rcv{ack_done = true},[],true};

parse(Data, State=#state_rcv{acc = []}) ->
    {State#state_rcv{acc=Data, datasize=size(Data)},[],false};

parse(Data, State=#state_rcv{acc = Bin, datasize=Size}) ->
	NewSize = Size + size(Data),
    parse(<< Bin/binary,Data/binary >>, State#state_rcv{acc=[], datasize=NewSize}).

parse_len({big_endian, BitSize}, Bin) ->
	case Bin of
		<<Len:BitSize/big,Rest/binary>> ->	{Len, Rest};
		_Any -> {0, Bin}
	end;

parse_len({little_endian, BitSize}, Bin) ->
	case Bin of
		<<Len:BitSize/little,Rest/binary>> ->	{Len, Rest};
		_Any -> {0, Bin}
	end;

parse_len({display, ByteSize}, Bin) ->
	case Bin of
		<<Len:ByteSize/binary,Rest/binary>> ->
			{list_to_integer(Len), Rest};
		_Any -> {0, Bin}
	end;

parse_len(_, Bin) ->
	{0, Bin}.

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
	ts_config_iso8583:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: we dont actually do anything
%% Args: Subst (true|false), DynData = #dyndata,
%% 		 Param = #ISO8583_request, Host  = String
%% Returns: #iso8583_request
%%----------------------------------------------------------------------
add_dynparams(false, _DynData, Param, _HostData) ->
    Param;

add_dynparams(true, DynData, Param, _HostData) ->
    subst(Param, DynData#dyndata.dynvars).

%%----------------------------------------------------------------------
%% Function: init_dynparams/0
%% Purpose:  initial dynamic parameters value
%% Returns:  #dyndata
%%----------------------------------------------------------------------
init_dynparams() ->
	#dyndata{proto=#iso8583_dyndata{}}.

%%----------------------------------------------------------------------
%% Function: subst/2
%% Purpose: Replace on the fly dynamic element of the request.
%%----------------------------------------------------------------------
subst(Req=#iso8583_request{fields=F}, DynData) ->
    Req#iso8583_request{
					fields = lists:foldl(fun ({Id, Value}, Result) ->
											[{Id, ts_search:subst(Value, DynData)} | Result]
										end, [], F)
	}.

%%
%% common iso8583 dynamic variables
%%
yymmdd(_) ->
	{{Year, Month, Day},_} = calendar:local_time(),
	lists:flatten(io_lib:format("~2..0w~2..0w~2..0w", [Year rem 100, Month, Day])).

hhmmss(_) ->
	{_,{Hours, Minutes, Seconds}} = calendar:local_time(),
	lists:flatten(io_lib:format("~2..0w~2..0w~2..0w", [Hours, Minutes, Seconds])).

yymmddhhmmss(_) ->
	{{Year, Month, Day},{Hours, Minutes, Seconds}} = calendar:local_time(),
	lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0w",
					[Year rem 100, Month, Day, Hours, Minutes, Seconds])).

mmddhhmmssUTC(_) ->
	{{_Year, Month, Day},{Hours, Minutes, Seconds}} = calendar:universal_time(),
	lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w",
					[Month, Day, Hours, Minutes, Seconds])).

unique_6(_) ->
	erlang:phash2({make_ref(),now()},1000000).

unique_12(_) ->
	lists:flatten(io_lib:format("~12..0w",
							[erlang:phash2({make_ref(),now()},4294967296)])).

