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
%% @doc Provides functions for building and parsing iso 8583 messages.
%%
%%	Messages are built and parsed according to the instructions
%%	detailed in an XML file. The supported XML format is the one used
%% 	in the jpos project ([http://www.jpos.org]).
%%
%% 	The internal format of an iso 8583 message is a list of tuples
%%	containing the field id and its value.
%% @end

-module(iso).

%%
%% @type	isomsg()	= list(Field)
%%						Field	= {Id, Value}
%%						Id		= integer().
%% ISO8583 message is modeled as a list of tuples where each tuple
%% represent an iso fileld. `Id' is the field id while `Value' is
%% the value of the field as specified in the interface configuration.
%%

-export([build_packager/1, get_MTI/1, get_field/2, set_field/3, set_MTI/2,
		pack/2, unpack/2, has_field/2, merge/2, remove/2, packager_for/1,
		to_hex/1, hex_to_bin/1]).

-include_lib("xmerl/include/xmerl.hrl").

%%--------------------------------------------------------------------
%% @spec	(FileOrXml) -> Packager
%%
%%				FileOrXml	= list() | #xmlElement{}
%%
%% @doc		Builds a Packager from a xml file specification or from
%% 			an already parsed xml file.
%%
%% 			Packager is used to parse and build iso 8583 binaries
%% 			to/from isomsg().
%%
%% 			Supported iso 8583 specification format is the one used
%% 			in the jpos ([http://www.jpos.org]) project.
%% @end
%%--------------------------------------------------------------------
build_packager(FileOrXml) when is_list(FileOrXml) ->
	{Xml, _} = xmerl_scan:file(FileOrXml),
	build_packager(Xml, dict:new());

build_packager(FileOrXml) when is_record(FileOrXml, xmlElement) ->
	build_packager(FileOrXml, dict:new()).

%% @hidden
build_packager(R, D) when is_record(R, xmlElement) ->
	case R#xmlElement.name of
		isopackager ->
			lists:foldl(fun build_packager/2, D, R#xmlElement.content);
		isofield ->
			Attr = lists:foldl(fun build_packager/2, dict:new(), R#xmlElement.attributes),
			dict:store(	list_to_integer(dict:fetch(id,Attr)),
						packager_for({list_to_integer(dict:fetch(length,Attr)),
									dict:fetch(name,Attr),
									lists:last(string:tokens(dict:fetch(class,Attr), "."))}),
						D);
		isofieldpackager ->
			%% not yet supported
			throw({fieldTypeNotSupported, isofieldpackager})			
	end;
build_packager(#xmlAttribute{name=N, value=V}, D) ->
	dict:store(N,V,D);
build_packager(_, D) -> D.

%%--------------------------------------------------------------------
%% @spec	(Packager, Msg) -> binary()
%%
%% 				Msg = isomsg()
%%
%% @doc		Builds the binary representation of the iso message Msg
%% 			according to the specification of the Packager.
%% @end
%%--------------------------------------------------------------------
pack(Packager, Msg) ->
	AllFields = bitmap_packer(Msg),
	pack(Packager, lists:keysort(1, AllFields), []).

%% @hidden
pack(_D, [], L) ->
	list_to_binary(lists:reverse(L));
pack(D, [{FldId, Value} | Rest], L) ->
	{Packer,_Unpacker} = dict:fetch(FldId, D),
	pack(D, Rest, [Packer(Value) | L]).

%%--------------------------------------------------------------------
%% @spec	(Packager, binary()) -> Msg
%%
%% 				Msg = isomsg()
%%
%% @doc		Parses the binary representation of the iso message Msg
%% 			according to the specification of the Packager.
%% @end
%%--------------------------------------------------------------------
unpack(Packager, Data) ->
	unpack(lists:keysort(1, dict:to_list(Packager)), Data, []).

%% @hidden
unpack([], _Data, F) ->
	F;
unpack([{1,{_Packer,Unpacker}} | Rest], Data, F) ->
	{Bitmap, Bin} = Unpacker(Data),
	%% Fields = [ {1, Bitmap} | F ],
	unpack(lists:filter(fun({X,_}) -> lists:member(X, Bitmap) end, Rest), Bin, F);
unpack([{FldId,{_Packer,Unpacker}} | Rest], Data, F) ->
	{Field, Bin} = Unpacker(Data),
	Fields = [ {FldId, Field} | F ],
	unpack(Rest, Bin, Fields).

%%--------------------------------------------------------------------
%% @spec	(FieldId, Msg) -> true | false
%%
%% 				FieldId 	= integer()
%% 				Msg 		= isomsg()
%%
%% @doc		Checks if the field FieldId is present in the message Msg.
%% @end
%%--------------------------------------------------------------------
has_field(FieldId, Msg) ->
	case lists:keysearch(FieldId,1,Msg) of
		{value, _} -> true;
		_Any -> false
	end.

%%--------------------------------------------------------------------
%% @spec	(Msg) -> list()
%%
%% 				Msg 		= isomsg()
%%
%% @doc		Returns the Message Type Identifier of the message Msg.
%% @end
%%--------------------------------------------------------------------
get_MTI(Msg) ->
	{value, {_, MTI}} = lists:keysearch(0,1,Msg),
	MTI.

%%--------------------------------------------------------------------
%% @spec	(FieldId, Msg) -> Value
%%
%% 				FieldId		= integer()
%% 				Msg 		= isomsg()
%%
%% @doc		Returns the value of the field FieldId.
%% @end
%%--------------------------------------------------------------------
get_field(FieldId,Msg) ->
	{value, {_, F}} = lists:keysearch(FieldId,1,Msg),
	F.

%%--------------------------------------------------------------------
%% @spec	(MTI, Msg) -> isomsg()
%%
%% 				Msg 		= isomsg()
%%
%% @doc		Sets the MTI to the message Msg.
%% @end
%%--------------------------------------------------------------------
set_MTI(MTI, Msg) ->
	[{0,MTI} | lists:keydelete(0,1,Msg)].
	
%%--------------------------------------------------------------------
%% @spec	(FieldId, Value, Msg) -> isomsg()
%%
%% 				FieldId		= integer()
%% 				Msg 		= isomsg()
%%
%% @doc		Sets the Value if the FieldId in the message Msg.
%% @end
%%--------------------------------------------------------------------
set_field(FieldId, Value, Msg) ->
	[{FieldId,Value} | lists:keydelete(FieldId,1,Msg)].

%%--------------------------------------------------------------------
%% @spec	(isomsg(), isomsg()) -> isomsg()
%%
%% @doc		Merges the field of the two messages into one.
%% 			In case a field is present in both messages only the
%% 			value of the first message is kept.
%% @end
%%--------------------------------------------------------------------
merge(M1, M2) ->
	lists:ukeymerge(1,
					lists:keysort(1,M1),
					lists:keysort(1,M2)).

%%--------------------------------------------------------------------
%% @spec	(FieldId, Msg) -> isomsg()
%%
%% 				FieldId		= integer()
%% 				Msg 		= isomsg()
%%
%% @doc		Removes the FieldId field in the message Msg.
%% @end
%%--------------------------------------------------------------------
remove(FieldId, Msg) ->
	lists:keydelete(FieldId,1,Msg).

%%
%% Return a packager tuple {Packer,Unpacker} according to the specification provided
%%	
packager_for(Specification) ->
	{packer_for(Specification), unpacker_for(Specification)}.

%%--------------------------------------------------------------------
%% @spec	(X) -> Hex
%%
%%				X 	= list() | binary()
%%				Hex = list() | binary()
%%
%% @doc		Convert X to its hexadecimal representation Hex.
%% @end
%%--------------------------------------------------------------------
to_hex(L) when is_list(L) ->  
	lists:flatten([int_to_hex(X) || X <- L]);
to_hex(B) when is_binary(B) ->
	list_to_binary([int_to_hex(X) || X <- binary_to_list(B)]).

int_to_hex(N) when N < 256 ->  
	[hex(N div 16), hex(N rem 16)].  
   
hex(N) when N < 10 ->  
	$0+N;  
hex(N) when N >= 10, N < 16 ->  
	$A + (N-10).

%%--------------------------------------------------------------------
%% @spec	(X) -> Hex
%%
%%				X 	= list() | binary()
%%				Hex = binary()
%%
%% @doc		Converts X to its binary representation.
%% @end
%%--------------------------------------------------------------------
hex_to_bin(L) when is_list(L) ->
	to_binl(L, []);
hex_to_bin(B) when is_binary(B) ->
	to_binb(B, []).

to_binl([], Acc) -> list_to_binary(lists:reverse(Acc));
to_binl([X,Y | L], Acc) when X < 256, Y  < 256 ->
	to_binl(L, [int(X)*16+int(Y) | Acc]);
to_binl([X], Acc) when X < 256 ->
	list_to_binary(lists:reverse([int(X)*16 | Acc])).

to_binb(<<>>, Acc) -> list_to_binary(lists:reverse(Acc));
to_binb(<<X,Y,B/binary>>, Acc) ->
	to_binb(B, [int(X)*16+int(Y) | Acc]);
to_binb(<<X>>, Acc) ->
	list_to_binary(lists:reverse([int(X)*16 | Acc])).

int(N) when N >= $0, N =< $9 ->
	N - $0;
int(N) when N >= $A, N =< $F ->
	10 + N - $A.

%%
%% packers
%%

bitmap_packer(L) ->
	Fields = [X || {X,_} <- L],
	Max = lists:max(Fields),
	Bits = if
		Max > 64 -> [1 | Fields];
		true -> Fields
	end,
	[{1, Bits} | L].

packer_for({_Len, _Name, "IFA_BITMAP"}) ->
	fun
	(Data) when is_list(Data) ->
		to_hex(bitList2bin(Data));
	(Data) when is_binary(Data) ->
		Data
	end;
packer_for({_Len, _Name, "IFB_BITMAP"}) ->
	fun
	(Data) when is_list(Data) ->
		bitList2bin(Data);
	(Data) when is_binary(Data) ->
		Data
	end;
packer_for({_Len, _Name, "IFA_BINARY"}) ->
	fun
	(Data) when is_list(Data) ->
		to_hex(Data);
	(Data) when is_binary(Data) ->
		to_hex(Data)
	end;
packer_for({_Len, _Name, "IFB_BINARY"}) ->
	fun (Data) ->
		Data
	end;
packer_for({_Len, _Name, "IFA_LLBINARY"}) ->
	fun
	(Data) when is_binary(Data) ->
		[string:right(integer_to_list(size(Data)*2), 2, $0), to_hex(Data)];	
	(Data) when is_list(Data) ->
		[string:right(integer_to_list(length(Data)*2), 2, $0), to_hex(Data)]	
	end;
packer_for({_Len, _Name, "IFA_LLLBINARY"}) ->
	fun
	(Data) when is_binary(Data) ->
		[string:right(integer_to_list(size(Data)*2), 3, $0), to_hex(Data)];
	(Data) when is_list(Data) ->
		[string:right(integer_to_list(length(Data)*2), 3, $0), to_hex(Data)]
	end;
packer_for({_Len, _Name, "IFB_LLBINARY"}) ->
	fun
	(Data) when is_binary(Data) ->
		[string:right(integer_to_list(size(Data)), 2, $0), Data];
	(Data) when is_list(Data) ->
		[string:right(integer_to_list(length(Data)), 2, $0), Data]
	end;
packer_for({_Len, _Name, "IFB_LLLBINARY"}) ->
	fun
	(Data) when is_binary(Data) ->
		[string:right(integer_to_list(size(Data)), 3, $0), Data];
	(Data) when is_list(Data) ->
		[string:right(integer_to_list(length(Data)), 3, $0), Data]
	end;
packer_for({Len, _Name, "IFA_NUMERIC"}) ->
	fun
	(Data) when is_list(Data) ->
		string:right(Data, Len, $0);	
	(Data) when is_integer(Data) ->
		string:right(integer_to_list(Data), Len, $0);
	(Data) when is_binary(Data) ->
		Data
	end;
packer_for({Len, _Name, "IFB_NUMERIC"}) ->
	fun
	(Data) when is_list(Data) ->
		hex_to_bin(string:right(Data, Len, $0));
	(Data) when is_integer(Data) ->
		hex_to_bin(string:right(integer_to_list(Data), Len, $0));
	(Data) when is_binary(Data) ->
		hex_to_bin(Data)
	end;
packer_for({Len, _Name, "IFA_AMOUNT"}) ->
	fun
	(Data) when is_list(Data) ->
		string:right(Data, Len, $0);
	(Data) when is_integer(Data) ->
		Sign = if
			Data < 0 -> $D;
			true -> $C
		end,
		[Sign|string:right(integer_to_list(abs(Data)), Len-1, $0)];
	(Data) when is_binary(Data) ->
		Data
	end;
packer_for({Len, _Name, "IFB_AMOUNT"}) ->
	fun
	(Data) when is_list(Data) ->
		hex_to_bin(string:right(Data, Len, $0));
	(Data) when is_integer(Data) ->
		Sign = if
			Data < 0 -> $D;
			true -> $C
		end,
		hex_to_bin([Sign|string:right(integer_to_list(abs(Data)), Len-1, $0)]);
	(Data) when is_binary(Data) ->
		hex_to_bin(Data)
	end;
packer_for({_Len, _Name, "IFA_LLNUM"}) ->
	fun
	(Data) when is_list(Data) ->
		[string:right(integer_to_list(length(Data)), 2, $0), Data];	
	(Data) when is_integer(Data) ->
		L = integer_to_list(Data),
		[string:right(integer_to_list(length(L)), 2, $0), L];
	(Data) when is_binary(Data) ->
		[string:right(integer_to_list(size(Data)), 2, $0), Data]
	end;
packer_for({_Len, _Name, "IFB_LLNUM"}) ->
	fun
	(Data) when is_list(Data) ->
		[string:right(integer_to_list(length(Data)), 2, $0), hex_to_bin(Data)];
	(Data) when is_integer(Data) ->
		L = integer_to_list(Data),
		[string:right(integer_to_list(length(L)), 2, $0), hex_to_bin(L)];
	(Data) when is_binary(Data) ->
		[string:right(integer_to_list(size(Data)), 2, $0), hex_to_bin(Data)]
	end;
packer_for({_Len, _Name, "IFA_LLLNUM"}) ->
	fun
	(Data) when is_list(Data) ->
		[string:right(integer_to_list(length(Data)), 3, $0), Data];	
	(Data) when is_integer(Data) ->
		L = integer_to_list(Data),
		[string:right(integer_to_list(length(L)), 3, $0), L];
	(Data) when is_binary(Data) ->
		[string:right(integer_to_list(size(Data)), 3, $0), Data]
	end;
packer_for({_Len, _Name, "IFB_LLLNUM"}) ->
	fun
	(Data) when is_list(Data) ->
		[string:right(integer_to_list(length(Data)), 3, $0), hex_to_bin(Data)];
	(Data) when is_integer(Data) ->
		L = integer_to_list(Data),
		[string:right(integer_to_list(length(L)), 3, $0), hex_to_bin(L)];
	(Data) when is_binary(Data) ->
		[string:right(integer_to_list(size(Data)), 3, $0), hex_to_bin(Data)]
	end;
packer_for({Len, _Name, "IF_CHAR"}) ->
	fun
	(Data) when is_list(Data) ->
		string:left(Data, Len, $\s);
	(Data) when is_binary(Data) ->
		Data
	end;
packer_for({_Len, _Name, "IFA_LLCHAR"}) ->
	fun
	(Data) when is_list(Data) ->
		[string:right(integer_to_list(length(Data)), 2, $0), Data];
	(Data) when is_binary(Data) ->
		[string:right(integer_to_list(size(Data)), 2, $0), Data]
	end;
packer_for({_Len, _Name, "IFA_LLLCHAR"}) ->
	fun
	(Data) when is_list(Data) ->
		[string:right(integer_to_list(length(Data)), 3, $0), Data];
	(Data) when is_binary(Data) ->
		[string:right(integer_to_list(size(Data)), 3, $0), Data]
	end;
packer_for({_Len, _Name, "IFE_LLLBINARY"}) ->
	fun
	(Data) when is_binary(Data) ->
		[ascii_to_ebcdic(string:right(integer_to_list(size(Data)), 3, $0)), Data];
	(Data) when is_list(Data) ->
		[ascii_to_ebcdic(string:right(integer_to_list(length(Data)), 3, $0)), Data]
	end;
packer_for({Len, _Name, "IFE_NUMERIC"}) ->
	fun
	(Data) when is_list(Data) ->
		ascii_to_ebcdic(string:right(Data, Len, $0));	
	(Data) when is_integer(Data) ->
		ascii_to_ebcdic(string:right(integer_to_list(Data), Len, $0));
	(Data) when is_binary(Data) ->
		ascii_to_ebcdic(Data)
	end;
packer_for({Len, _Name, "IFE_AMOUNT"}) ->
	fun
	(Data) when is_list(Data) ->
		ascii_to_ebcdic(string:right(Data, Len, $0));
	(Data) when is_integer(Data) ->
		Sign = if
			Data < 0 -> $D;
			true -> $C
		end,
		ascii_to_ebcdic([Sign|string:right(integer_to_list(abs(Data)), Len-1, $0)]);
	(Data) when is_binary(Data) ->
		ascii_to_ebcdic(Data)
	end;
packer_for({_Len, _Name, "IFE_LLNUM"}) ->
	fun
	(Data) when is_list(Data) ->
		ascii_to_ebcdic([string:right(integer_to_list(length(Data)), 2, $0), Data]);	
	(Data) when is_integer(Data) ->
		L = integer_to_list(Data),
		ascii_to_ebcdic([string:right(integer_to_list(length(L)), 2, $0), L]);
	(Data) when is_binary(Data) ->
		ascii_to_ebcdic([string:right(integer_to_list(size(Data)), 2, $0), Data])
	end;
packer_for({_Len, _Name, "IFE_LLLNUM"}) ->
	fun
	(Data) when is_list(Data) ->
		ascii_to_ebcdic([string:right(integer_to_list(length(Data)), 3, $0), Data]);	
	(Data) when is_integer(Data) ->
		L = integer_to_list(Data),
		ascii_to_ebcdic([string:right(integer_to_list(length(L)), 3, $0), L]);
	(Data) when is_binary(Data) ->
		ascii_to_ebcdic([string:right(integer_to_list(size(Data)), 3, $0), Data])
	end;
packer_for({Len, _Name, "IF_ECHAR"}) ->
	fun
	(Data) when is_list(Data) ->
		ascii_to_ebcdic(string:left(Data, Len, $\s));
	(Data) when is_binary(Data) ->
		ascii_to_ebcdic(Data)
	end;
packer_for({_Len, _Name, "IFE_LLCHAR"}) ->
	fun
	(Data) when is_list(Data) ->
		ascii_to_ebcdic([string:right(integer_to_list(length(Data)), 2, $0), Data]);
	(Data) when is_binary(Data) ->
		ascii_to_ebcdic([string:right(integer_to_list(size(Data)), 2, $0), Data])
	end;
packer_for({_Len, _Name, "IFE_LLLCHAR"}) ->
	fun
	(Data) when is_list(Data) ->
		ascii_to_ebcdic([string:right(integer_to_list(length(Data)), 3, $0), Data]);
	(Data) when is_binary(Data) ->
		ascii_to_ebcdic([string:right(integer_to_list(size(Data)), 3, $0), Data])
	end;
packer_for({_Len, Name, Class}) ->
	throw({unknownPackagerClass, Name, Class}).


%%
%% unpackers
%%

unpacker_for({_Len, _Name, "IFA_BITMAP"}) ->
	fun(Data) ->
		<<Bits:16/binary, Rest/binary>> = Data,
		PB = bin2bitList(hex_to_bin(Bits), 1, []),
		SecBitmapBit = lists:last(PB),
		if
			SecBitmapBit == 1 ->
				<<Bits2:16/binary, Rest2/binary>> = Rest,
				{bin2bitList(hex_to_bin(Bits2), 65, PB), Rest2};
			true -> {PB, Rest}
		end
	end;
unpacker_for({_Len, _Name, "IFB_BITMAP"}) ->
	fun(Data) ->
		<<Bits:8/binary, Rest/binary>> = Data,
		PB = bin2bitList(Bits, 1, []),
		SecBitmapBit = lists:last(PB),
		if
			SecBitmapBit == 1 ->
				<<Bits2:8/binary, Rest2/binary>> = Rest,
				{bin2bitList(Bits2, 65, PB), Rest2};
			true -> {PB, Rest}
		end
	end;
unpacker_for({Len, _Name, "IFA_BINARY"}) ->
	fun(Data) ->
		Size = Len*2,
		<<Field:Size/binary, Rest/binary>> = Data,
		{hex_to_bin(Field), Rest}
	end;
unpacker_for({_Len, _Name, "IFA_LLBINARY"}) ->
	fun(Data) ->
		<<LL:2/binary, Bin/binary>> = Data,
		Size = list_to_integer(binary_to_list(LL)),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{hex_to_bin(Field), Rest}
	end;
unpacker_for({_Len, _Name, "IFA_LLLBINARY"}) ->
	fun(Data) ->
		<<LL:3/binary, Bin/binary>> = Data,
		Size = list_to_integer(binary_to_list(LL)),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{hex_to_bin(Field), Rest}
	end;
unpacker_for({Len, _Name, "IFB_BINARY"}) ->
	fun(Data) ->
		<<Field:Len/binary, Rest/binary>> = Data,
		{Field, Rest}
	end;
unpacker_for({_Len, _Name, "IFB_LLBINARY"}) ->
	fun(Data) ->
		<<LL:2/binary, Bin/binary>> = Data,
		Size = list_to_integer(binary_to_list(LL)),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{Field, Rest}
	end;
unpacker_for({_Len, _Name, "IFB_LLLBINARY"}) ->
	fun(Data) ->
		<<LL:3/binary, Bin/binary>> = Data,
		Size = list_to_integer(binary_to_list(LL)),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{Field, Rest}
	end;
unpacker_for({Len, _Name, "IFA_NUMERIC"}) ->
	fun(Data) ->
		<<Field:Len/binary, Rest/binary>> = Data,
		{Field, Rest}
	end;
unpacker_for({Len, _Name, "IFB_NUMERIC"}) ->
	fun(Data) ->
		S = (Len + (Len rem 2)) div 2,
		<<Field0:S/binary, Rest/binary>> = Data,
		<<Field:Len/binary,_/binary>> = to_hex(Field0),
		{Field, Rest}
	end;
unpacker_for({Len, _Name, "IFA_AMOUNT"}) ->
	fun(Data) ->
		<<Field:Len/binary, Rest/binary>> = Data,
		{Field, Rest}
	end;
unpacker_for({Len, _Name, "IFB_AMOUNT"}) ->
	fun(Data) ->
		S = (Len + (Len rem 2)) div 2,
		<<Field0:S/binary, Rest/binary>> = Data,
		<<Field:Len/binary,_/binary>> = to_hex(Field0),
		{Field, Rest}
	end;
unpacker_for({_Len, _Name, "IFA_LLNUM"}) ->
	fun(Data) ->
		<<LL:2/binary, Bin/binary>> = Data,
		Size = list_to_integer(binary_to_list(LL)),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{Field, Rest}
	end;
unpacker_for({_Len, _Name, "IFB_LLNUM"}) ->
	fun(Data) ->
		<<LL:2/binary, Bin/binary>> = Data,
		Size = list_to_integer(binary_to_list(LL)),
		S = (Size + (Size rem 2)) div 2,
		<<Field0:S/binary, Rest/binary>> = Bin,
		<<Field:Size/binary,_/binary>> = to_hex(Field0),
		{Field, Rest}
	end;
unpacker_for({_Len, _Name, "IFA_LLLNUM"}) ->
	fun(Data) ->
		<<LLL:3/binary, Bin/binary>> = Data,
		Size = list_to_integer(binary_to_list(LLL)),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{Field, Rest}
	end;
unpacker_for({_Len, _Name, "IFB_LLLNUM"}) ->
	fun(Data) ->
		<<LLL:3/binary, Bin/binary>> = Data,
		Size = list_to_integer(binary_to_list(LLL)),
		S = (Size + (Size rem 2)) div 2,
		<<Field0:S/binary, Rest/binary>> = Bin,
		<<Field:Size/binary,_/binary>> = to_hex(Field0),
		{Field, Rest}
	end;
unpacker_for({Len, _Name, "IF_CHAR"}) ->
	fun(Data) ->
		<<Field:Len/binary, Rest/binary>> = Data,
		{Field, Rest}
	end;
unpacker_for({_Len, _Name, "IFA_LLCHAR"}) ->
	fun(Data) ->
		<<LL:2/binary, Bin/binary>> = Data,
		Size = list_to_integer(binary_to_list(LL)),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{Field, Rest}
	end;
unpacker_for({_Len, _Name, "IFA_LLLCHAR"}) ->
	fun(Data) ->
		<<LLL:3/binary, Bin/binary>> = Data,
		Size = list_to_integer(binary_to_list(LLL)),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{Field, Rest}
	end;
unpacker_for({_Len, _Name, "IFE_LLLBINARY"}) ->
	fun(Data) ->
		<<LL:3/binary, Bin/binary>> = Data,
		Size = list_to_integer(ebcdic_to_ascii(binary_to_list(LL))),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{Field, Rest}
	end;
unpacker_for({Len, _Name, "IFE_NUMERIC"}) ->
	fun(Data) ->
		<<Field:Len/binary, Rest/binary>> = Data,
		{ebcdic_to_ascii(Field), Rest}
	end;
unpacker_for({Len, _Name, "IFE_AMOUNT"}) ->
	fun(Data) ->
		<<Field:Len/binary, Rest/binary>> = Data,
		{ebcdic_to_ascii(Field), Rest}
	end;
unpacker_for({_Len, _Name, "IFE_LLNUM"}) ->
	fun(Data) ->
		<<LL:2/binary, Bin/binary>> = Data,
		Size = list_to_integer(ebcdic_to_ascii(binary_to_list(LL))),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{ebcdic_to_ascii(Field), Rest}
	end;
unpacker_for({_Len, _Name, "IFE_LLLNUM"}) ->
	fun(Data) ->
		<<LLL:3/binary, Bin/binary>> = Data,
		Size = list_to_integer(ebcdic_to_ascii(binary_to_list(LLL))),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{ebcdic_to_ascii(Field), Rest}
	end;
unpacker_for({Len, _Name, "IF_ECHAR"}) ->
	fun(Data) ->
		<<Field:Len/binary, Rest/binary>> = Data,
		{ebcdic_to_ascii(Field), Rest}
	end;
unpacker_for({_Len, _Name, "IFE_LLCHAR"}) ->
	fun(Data) ->
		<<LL:2/binary, Bin/binary>> = Data,
		Size = list_to_integer(ebcdic_to_ascii(binary_to_list(LL))),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{ebcdic_to_ascii(Field), Rest}
	end;
unpacker_for({_Len, _Name, "IFE_LLLCHAR"}) ->
	fun(Data) ->
		<<LLL:3/binary, Bin/binary>> = Data,
		Size = list_to_integer(ebcdic_to_ascii(binary_to_list(LLL))),
		<<Field:Size/binary, Rest/binary>> = Bin,
		{ebcdic_to_ascii(Field), Rest}
	end;
unpacker_for({_Len, Name, Class}) ->
	throw({unknownPackagerClass, Name, Class}).

%%
%% Convert the bitmap in a list of 1s and 0s
%%

bin2bitList(<<>>, _, L) ->
	[X || X <- L, X > 0];
bin2bitList(<<B1:1,B2:1,B3:1,B4:1,B5:1,B6:1,B7:1,B8:1,Rest/binary>>, N, L) ->
	bin2bitList(Rest, N+8,
	[B8*(N+7),B7*(N+6),B6*(N+5),B5*(N+4),B4*(N+3),B3*(N+2),B2*(N+1),B1*N | L]).

bitList2bin(L) ->
	Max = lists:max(L),
	if
		Max > 64 -> Size = 128;
		true -> Size = 64
	end,
	bitList2bin([bool_to_int(lists:member(X,L)) || X <- lists:seq(1, Size)], []).

bool_to_int(true) -> 1;
bool_to_int(false) -> 0.

bitList2bin([], L) ->
	list_to_binary(lists:reverse(L));
bitList2bin([X1,X2,X3,X4,X5,X6,X7,X8 | Rest], L) ->
	bitList2bin(Rest, [<<X1:1,X2:1,X3:1,X4:1,X5:1,X6:1,X7:1,X8:1>> | L]).


ebcdic_to_ascii(0) -> 16#0;
ebcdic_to_ascii(1) -> 16#1;
ebcdic_to_ascii(2) -> 16#2;
ebcdic_to_ascii(3) -> 16#3;
ebcdic_to_ascii(4) -> 16#9C;
ebcdic_to_ascii(5) -> 16#9;
ebcdic_to_ascii(6) -> 16#86;
ebcdic_to_ascii(7) -> 16#7F;
ebcdic_to_ascii(8) -> 16#97;
ebcdic_to_ascii(9) -> 16#8D;
ebcdic_to_ascii(10) -> 16#8E;
ebcdic_to_ascii(11) -> 16#B;
ebcdic_to_ascii(12) -> 16#C;
ebcdic_to_ascii(13) -> 16#D;
ebcdic_to_ascii(14) -> 16#E;
ebcdic_to_ascii(15) -> 16#F;
ebcdic_to_ascii(16) -> 16#10;
ebcdic_to_ascii(17) -> 16#11;
ebcdic_to_ascii(18) -> 16#12;
ebcdic_to_ascii(19) -> 16#13;
ebcdic_to_ascii(20) -> 16#9D;
ebcdic_to_ascii(21) -> 16#A;
ebcdic_to_ascii(22) -> 16#8;
ebcdic_to_ascii(23) -> 16#87;
ebcdic_to_ascii(24) -> 16#18;
ebcdic_to_ascii(25) -> 16#19;
ebcdic_to_ascii(26) -> 16#92;
ebcdic_to_ascii(27) -> 16#8F;
ebcdic_to_ascii(28) -> 16#1C;
ebcdic_to_ascii(29) -> 16#1D;
ebcdic_to_ascii(30) -> 16#1E;
ebcdic_to_ascii(31) -> 16#1F;
ebcdic_to_ascii(32) -> 16#80;
ebcdic_to_ascii(33) -> 16#81;
ebcdic_to_ascii(34) -> 16#82;
ebcdic_to_ascii(35) -> 16#83;
ebcdic_to_ascii(36) -> 16#84;
ebcdic_to_ascii(37) -> 16#85;
ebcdic_to_ascii(38) -> 16#17;
ebcdic_to_ascii(39) -> 16#1B;
ebcdic_to_ascii(40) -> 16#88;
ebcdic_to_ascii(41) -> 16#89;
ebcdic_to_ascii(42) -> 16#8A;
ebcdic_to_ascii(43) -> 16#8B;
ebcdic_to_ascii(44) -> 16#8C;
ebcdic_to_ascii(45) -> 16#5;
ebcdic_to_ascii(46) -> 16#6;
ebcdic_to_ascii(47) -> 16#7;
ebcdic_to_ascii(48) -> 16#90;
ebcdic_to_ascii(49) -> 16#91;
ebcdic_to_ascii(50) -> 16#16;
ebcdic_to_ascii(51) -> 16#93;
ebcdic_to_ascii(52) -> 16#94;
ebcdic_to_ascii(53) -> 16#95;
ebcdic_to_ascii(54) -> 16#96;
ebcdic_to_ascii(55) -> 16#4;
ebcdic_to_ascii(56) -> 16#98;
ebcdic_to_ascii(57) -> 16#99;
ebcdic_to_ascii(58) -> 16#9A;
ebcdic_to_ascii(59) -> 16#9B;
ebcdic_to_ascii(60) -> 16#14;
ebcdic_to_ascii(61) -> 16#15;
ebcdic_to_ascii(62) -> 16#9E;
ebcdic_to_ascii(63) -> 16#1A;
ebcdic_to_ascii(64) -> 16#20;
ebcdic_to_ascii(65) -> 16#A0;
ebcdic_to_ascii(66) -> 16#E2;
ebcdic_to_ascii(67) -> 16#E4;
ebcdic_to_ascii(68) -> 16#E0;
ebcdic_to_ascii(69) -> 16#E1;
ebcdic_to_ascii(70) -> 16#E3;
ebcdic_to_ascii(71) -> 16#E5;
ebcdic_to_ascii(72) -> 16#E7;
ebcdic_to_ascii(73) -> 16#F1;
ebcdic_to_ascii(74) -> 16#A2;
ebcdic_to_ascii(75) -> 16#2E;
ebcdic_to_ascii(76) -> 16#3C;
ebcdic_to_ascii(77) -> 16#28;
ebcdic_to_ascii(78) -> 16#2B;
ebcdic_to_ascii(79) -> 16#7C;
ebcdic_to_ascii(80) -> 16#26;
ebcdic_to_ascii(81) -> 16#E9;
ebcdic_to_ascii(82) -> 16#EA;
ebcdic_to_ascii(83) -> 16#EB;
ebcdic_to_ascii(84) -> 16#E8;
ebcdic_to_ascii(85) -> 16#ED;
ebcdic_to_ascii(86) -> 16#EE;
ebcdic_to_ascii(87) -> 16#EF;
ebcdic_to_ascii(88) -> 16#EC;
ebcdic_to_ascii(89) -> 16#DF;
ebcdic_to_ascii(90) -> 16#21;
ebcdic_to_ascii(91) -> 16#24;
ebcdic_to_ascii(92) -> 16#2A;
ebcdic_to_ascii(93) -> 16#29;
ebcdic_to_ascii(94) -> 16#3B;
ebcdic_to_ascii(95) -> 16#5E;
ebcdic_to_ascii(96) -> 16#2D;
ebcdic_to_ascii(97) -> 16#2F;
ebcdic_to_ascii(98) -> 16#C2;
ebcdic_to_ascii(99) -> 16#C4;
ebcdic_to_ascii(100) -> 16#C0;
ebcdic_to_ascii(101) -> 16#C1;
ebcdic_to_ascii(102) -> 16#C3;
ebcdic_to_ascii(103) -> 16#C5;
ebcdic_to_ascii(104) -> 16#C7;
ebcdic_to_ascii(105) -> 16#D1;
ebcdic_to_ascii(106) -> 16#A6;
ebcdic_to_ascii(107) -> 16#2C;
ebcdic_to_ascii(108) -> 16#25;
ebcdic_to_ascii(109) -> 16#5F;
ebcdic_to_ascii(110) -> 16#3E;
ebcdic_to_ascii(111) -> 16#3F;
ebcdic_to_ascii(112) -> 16#F8;
ebcdic_to_ascii(113) -> 16#C9;
ebcdic_to_ascii(114) -> 16#CA;
ebcdic_to_ascii(115) -> 16#CB;
ebcdic_to_ascii(116) -> 16#C8;
ebcdic_to_ascii(117) -> 16#CD;
ebcdic_to_ascii(118) -> 16#CE;
ebcdic_to_ascii(119) -> 16#CF;
ebcdic_to_ascii(120) -> 16#CC;
ebcdic_to_ascii(121) -> 16#60;
ebcdic_to_ascii(122) -> 16#3A;
ebcdic_to_ascii(123) -> 16#23;
ebcdic_to_ascii(124) -> 16#40;
ebcdic_to_ascii(125) -> 16#27;
ebcdic_to_ascii(126) -> 16#3D;
ebcdic_to_ascii(127) -> 16#22;
ebcdic_to_ascii(128) -> 16#D8;
ebcdic_to_ascii(129) -> 16#61;
ebcdic_to_ascii(130) -> 16#62;
ebcdic_to_ascii(131) -> 16#63;
ebcdic_to_ascii(132) -> 16#64;
ebcdic_to_ascii(133) -> 16#65;
ebcdic_to_ascii(134) -> 16#66;
ebcdic_to_ascii(135) -> 16#67;
ebcdic_to_ascii(136) -> 16#68;
ebcdic_to_ascii(137) -> 16#69;
ebcdic_to_ascii(138) -> 16#AB;
ebcdic_to_ascii(139) -> 16#BB;
ebcdic_to_ascii(140) -> 16#F0;
ebcdic_to_ascii(141) -> 16#FD;
ebcdic_to_ascii(142) -> 16#FE;
ebcdic_to_ascii(143) -> 16#B1;
ebcdic_to_ascii(144) -> 16#B0;
ebcdic_to_ascii(145) -> 16#6A;
ebcdic_to_ascii(146) -> 16#6B;
ebcdic_to_ascii(147) -> 16#6C;
ebcdic_to_ascii(148) -> 16#6D;
ebcdic_to_ascii(149) -> 16#6E;
ebcdic_to_ascii(150) -> 16#6F;
ebcdic_to_ascii(151) -> 16#70;
ebcdic_to_ascii(152) -> 16#71;
ebcdic_to_ascii(153) -> 16#72;
ebcdic_to_ascii(154) -> 16#AA;
ebcdic_to_ascii(155) -> 16#BA;
ebcdic_to_ascii(156) -> 16#E6;
ebcdic_to_ascii(157) -> 16#B8;
ebcdic_to_ascii(158) -> 16#C6;
ebcdic_to_ascii(159) -> 16#A4;
ebcdic_to_ascii(160) -> 16#B5;
ebcdic_to_ascii(161) -> 16#7E;
ebcdic_to_ascii(162) -> 16#73;
ebcdic_to_ascii(163) -> 16#74;
ebcdic_to_ascii(164) -> 16#75;
ebcdic_to_ascii(165) -> 16#76;
ebcdic_to_ascii(166) -> 16#77;
ebcdic_to_ascii(167) -> 16#78;
ebcdic_to_ascii(168) -> 16#79;
ebcdic_to_ascii(169) -> 16#7A;
ebcdic_to_ascii(170) -> 16#A1;
ebcdic_to_ascii(171) -> 16#BF;
ebcdic_to_ascii(172) -> 16#D0;
ebcdic_to_ascii(173) -> 16#5B;
ebcdic_to_ascii(174) -> 16#DE;
ebcdic_to_ascii(175) -> 16#AE;
ebcdic_to_ascii(176) -> 16#AC;
ebcdic_to_ascii(177) -> 16#A3;
ebcdic_to_ascii(178) -> 16#A5;
ebcdic_to_ascii(179) -> 16#B7;
ebcdic_to_ascii(180) -> 16#A9;
ebcdic_to_ascii(181) -> 16#A7;
ebcdic_to_ascii(182) -> 16#B6;
ebcdic_to_ascii(183) -> 16#BC;
ebcdic_to_ascii(184) -> 16#BD;
ebcdic_to_ascii(185) -> 16#BE;
ebcdic_to_ascii(186) -> 16#DD;
ebcdic_to_ascii(187) -> 16#A8;
ebcdic_to_ascii(188) -> 16#AF;
ebcdic_to_ascii(189) -> 16#5D;
ebcdic_to_ascii(190) -> 16#B4;
ebcdic_to_ascii(191) -> 16#D7;
ebcdic_to_ascii(192) -> 16#7B;
ebcdic_to_ascii(193) -> 16#41;
ebcdic_to_ascii(194) -> 16#42;
ebcdic_to_ascii(195) -> 16#43;
ebcdic_to_ascii(196) -> 16#44;
ebcdic_to_ascii(197) -> 16#45;
ebcdic_to_ascii(198) -> 16#46;
ebcdic_to_ascii(199) -> 16#47;
ebcdic_to_ascii(200) -> 16#48;
ebcdic_to_ascii(201) -> 16#49;
ebcdic_to_ascii(202) -> 16#AD;
ebcdic_to_ascii(203) -> 16#F4;
ebcdic_to_ascii(204) -> 16#F6;
ebcdic_to_ascii(205) -> 16#F2;
ebcdic_to_ascii(206) -> 16#F3;
ebcdic_to_ascii(207) -> 16#F5;
ebcdic_to_ascii(208) -> 16#7D;
ebcdic_to_ascii(209) -> 16#4A;
ebcdic_to_ascii(210) -> 16#4B;
ebcdic_to_ascii(211) -> 16#4C;
ebcdic_to_ascii(212) -> 16#4D;
ebcdic_to_ascii(213) -> 16#4E;
ebcdic_to_ascii(214) -> 16#4F;
ebcdic_to_ascii(215) -> 16#50;
ebcdic_to_ascii(216) -> 16#51;
ebcdic_to_ascii(217) -> 16#52;
ebcdic_to_ascii(218) -> 16#B9;
ebcdic_to_ascii(219) -> 16#FB;
ebcdic_to_ascii(220) -> 16#FC;
ebcdic_to_ascii(221) -> 16#F9;
ebcdic_to_ascii(222) -> 16#FA;
ebcdic_to_ascii(223) -> 16#FF;
ebcdic_to_ascii(224) -> 16#5C;
ebcdic_to_ascii(225) -> 16#F7;
ebcdic_to_ascii(226) -> 16#53;
ebcdic_to_ascii(227) -> 16#54;
ebcdic_to_ascii(228) -> 16#55;
ebcdic_to_ascii(229) -> 16#56;
ebcdic_to_ascii(230) -> 16#57;
ebcdic_to_ascii(231) -> 16#58;
ebcdic_to_ascii(232) -> 16#59;
ebcdic_to_ascii(233) -> 16#5A;
ebcdic_to_ascii(234) -> 16#B2;
ebcdic_to_ascii(235) -> 16#D4;
ebcdic_to_ascii(236) -> 16#D6;
ebcdic_to_ascii(237) -> 16#D2;
ebcdic_to_ascii(238) -> 16#D3;
ebcdic_to_ascii(239) -> 16#D5;
ebcdic_to_ascii(240) -> 16#30;
ebcdic_to_ascii(241) -> 16#31;
ebcdic_to_ascii(242) -> 16#32;
ebcdic_to_ascii(243) -> 16#33;
ebcdic_to_ascii(244) -> 16#34;
ebcdic_to_ascii(245) -> 16#35;
ebcdic_to_ascii(246) -> 16#36;
ebcdic_to_ascii(247) -> 16#37;
ebcdic_to_ascii(248) -> 16#38;
ebcdic_to_ascii(249) -> 16#39;
ebcdic_to_ascii(250) -> 16#B3;
ebcdic_to_ascii(251) -> 16#DB;
ebcdic_to_ascii(252) -> 16#DC;
ebcdic_to_ascii(253) -> 16#D9;
ebcdic_to_ascii(254) -> 16#DA;
ebcdic_to_ascii(255) -> 16#9F;
ebcdic_to_ascii(L) when is_list(L) ->
	[ebcdic_to_ascii(X) || X <- L];
ebcdic_to_ascii(B) when is_binary(B) ->
	list_to_binary([ebcdic_to_ascii(X) || X <- binary_to_list(B)]).

ascii_to_ebcdic(0) -> 16#0;
ascii_to_ebcdic(1) -> 16#1;
ascii_to_ebcdic(2) -> 16#2;
ascii_to_ebcdic(3) -> 16#3;
ascii_to_ebcdic(4) -> 16#37;
ascii_to_ebcdic(5) -> 16#2D;
ascii_to_ebcdic(6) -> 16#2E;
ascii_to_ebcdic(7) -> 16#2F;
ascii_to_ebcdic(8) -> 16#16;
ascii_to_ebcdic(9) -> 16#5;
ascii_to_ebcdic(10) -> 16#15;
ascii_to_ebcdic(11) -> 16#B;
ascii_to_ebcdic(12) -> 16#C;
ascii_to_ebcdic(13) -> 16#D;
ascii_to_ebcdic(14) -> 16#E;
ascii_to_ebcdic(15) -> 16#F;
ascii_to_ebcdic(16) -> 16#10;
ascii_to_ebcdic(17) -> 16#11;
ascii_to_ebcdic(18) -> 16#12;
ascii_to_ebcdic(19) -> 16#13;
ascii_to_ebcdic(20) -> 16#3C;
ascii_to_ebcdic(21) -> 16#3D;
ascii_to_ebcdic(22) -> 16#32;
ascii_to_ebcdic(23) -> 16#26;
ascii_to_ebcdic(24) -> 16#18;
ascii_to_ebcdic(25) -> 16#19;
ascii_to_ebcdic(26) -> 16#3F;
ascii_to_ebcdic(27) -> 16#27;
ascii_to_ebcdic(28) -> 16#1C;
ascii_to_ebcdic(29) -> 16#1D;
ascii_to_ebcdic(30) -> 16#1E;
ascii_to_ebcdic(31) -> 16#1F;
ascii_to_ebcdic(32) -> 16#40;
ascii_to_ebcdic(33) -> 16#5A;
ascii_to_ebcdic(34) -> 16#7F;
ascii_to_ebcdic(35) -> 16#7B;
ascii_to_ebcdic(36) -> 16#5B;
ascii_to_ebcdic(37) -> 16#6C;
ascii_to_ebcdic(38) -> 16#50;
ascii_to_ebcdic(39) -> 16#7D;
ascii_to_ebcdic(40) -> 16#4D;
ascii_to_ebcdic(41) -> 16#5D;
ascii_to_ebcdic(42) -> 16#5C;
ascii_to_ebcdic(43) -> 16#4E;
ascii_to_ebcdic(44) -> 16#6B;
ascii_to_ebcdic(45) -> 16#60;
ascii_to_ebcdic(46) -> 16#4B;
ascii_to_ebcdic(47) -> 16#61;
ascii_to_ebcdic(48) -> 16#F0;
ascii_to_ebcdic(49) -> 16#F1;
ascii_to_ebcdic(50) -> 16#F2;
ascii_to_ebcdic(51) -> 16#F3;
ascii_to_ebcdic(52) -> 16#F4;
ascii_to_ebcdic(53) -> 16#F5;
ascii_to_ebcdic(54) -> 16#F6;
ascii_to_ebcdic(55) -> 16#F7;
ascii_to_ebcdic(56) -> 16#F8;
ascii_to_ebcdic(57) -> 16#F9;
ascii_to_ebcdic(58) -> 16#7A;
ascii_to_ebcdic(59) -> 16#5E;
ascii_to_ebcdic(60) -> 16#4C;
ascii_to_ebcdic(61) -> 16#7E;
ascii_to_ebcdic(62) -> 16#6E;
ascii_to_ebcdic(63) -> 16#6F;
ascii_to_ebcdic(64) -> 16#7C;
ascii_to_ebcdic(65) -> 16#C1;
ascii_to_ebcdic(66) -> 16#C2;
ascii_to_ebcdic(67) -> 16#C3;
ascii_to_ebcdic(68) -> 16#C4;
ascii_to_ebcdic(69) -> 16#C5;
ascii_to_ebcdic(70) -> 16#C6;
ascii_to_ebcdic(71) -> 16#C7;
ascii_to_ebcdic(72) -> 16#C8;
ascii_to_ebcdic(73) -> 16#C9;
ascii_to_ebcdic(74) -> 16#D1;
ascii_to_ebcdic(75) -> 16#D2;
ascii_to_ebcdic(76) -> 16#D3;
ascii_to_ebcdic(77) -> 16#D4;
ascii_to_ebcdic(78) -> 16#D5;
ascii_to_ebcdic(79) -> 16#D6;
ascii_to_ebcdic(80) -> 16#D7;
ascii_to_ebcdic(81) -> 16#D8;
ascii_to_ebcdic(82) -> 16#D9;
ascii_to_ebcdic(83) -> 16#E2;
ascii_to_ebcdic(84) -> 16#E3;
ascii_to_ebcdic(85) -> 16#E4;
ascii_to_ebcdic(86) -> 16#E5;
ascii_to_ebcdic(87) -> 16#E6;
ascii_to_ebcdic(88) -> 16#E7;
ascii_to_ebcdic(89) -> 16#E8;
ascii_to_ebcdic(90) -> 16#E9;
ascii_to_ebcdic(91) -> 16#AD;
ascii_to_ebcdic(92) -> 16#E0;
ascii_to_ebcdic(93) -> 16#BD;
ascii_to_ebcdic(94) -> 16#5F;
ascii_to_ebcdic(95) -> 16#6D;
ascii_to_ebcdic(96) -> 16#79;
ascii_to_ebcdic(97) -> 16#81;
ascii_to_ebcdic(98) -> 16#82;
ascii_to_ebcdic(99) -> 16#83;
ascii_to_ebcdic(100) -> 16#84;
ascii_to_ebcdic(101) -> 16#85;
ascii_to_ebcdic(102) -> 16#86;
ascii_to_ebcdic(103) -> 16#87;
ascii_to_ebcdic(104) -> 16#88;
ascii_to_ebcdic(105) -> 16#89;
ascii_to_ebcdic(106) -> 16#91;
ascii_to_ebcdic(107) -> 16#92;
ascii_to_ebcdic(108) -> 16#93;
ascii_to_ebcdic(109) -> 16#94;
ascii_to_ebcdic(110) -> 16#95;
ascii_to_ebcdic(111) -> 16#96;
ascii_to_ebcdic(112) -> 16#97;
ascii_to_ebcdic(113) -> 16#98;
ascii_to_ebcdic(114) -> 16#99;
ascii_to_ebcdic(115) -> 16#A2;
ascii_to_ebcdic(116) -> 16#A3;
ascii_to_ebcdic(117) -> 16#A4;
ascii_to_ebcdic(118) -> 16#A5;
ascii_to_ebcdic(119) -> 16#A6;
ascii_to_ebcdic(120) -> 16#A7;
ascii_to_ebcdic(121) -> 16#A8;
ascii_to_ebcdic(122) -> 16#A9;
ascii_to_ebcdic(123) -> 16#C0;
ascii_to_ebcdic(124) -> 16#4F;
ascii_to_ebcdic(125) -> 16#D0;
ascii_to_ebcdic(126) -> 16#A1;
ascii_to_ebcdic(127) -> 16#7;
ascii_to_ebcdic(128) -> 16#20;
ascii_to_ebcdic(129) -> 16#21;
ascii_to_ebcdic(130) -> 16#22;
ascii_to_ebcdic(131) -> 16#23;
ascii_to_ebcdic(132) -> 16#24;
ascii_to_ebcdic(133) -> 16#25;
ascii_to_ebcdic(134) -> 16#6;
ascii_to_ebcdic(135) -> 16#17;
ascii_to_ebcdic(136) -> 16#28;
ascii_to_ebcdic(137) -> 16#29;
ascii_to_ebcdic(138) -> 16#2A;
ascii_to_ebcdic(139) -> 16#2B;
ascii_to_ebcdic(140) -> 16#2C;
ascii_to_ebcdic(141) -> 16#9;
ascii_to_ebcdic(142) -> 16#A;
ascii_to_ebcdic(143) -> 16#1B;
ascii_to_ebcdic(144) -> 16#30;
ascii_to_ebcdic(145) -> 16#31;
ascii_to_ebcdic(146) -> 16#1A;
ascii_to_ebcdic(147) -> 16#33;
ascii_to_ebcdic(148) -> 16#34;
ascii_to_ebcdic(149) -> 16#35;
ascii_to_ebcdic(150) -> 16#36;
ascii_to_ebcdic(151) -> 16#8;
ascii_to_ebcdic(152) -> 16#38;
ascii_to_ebcdic(153) -> 16#39;
ascii_to_ebcdic(154) -> 16#3A;
ascii_to_ebcdic(155) -> 16#3B;
ascii_to_ebcdic(156) -> 16#4;
ascii_to_ebcdic(157) -> 16#14;
ascii_to_ebcdic(158) -> 16#3E;
ascii_to_ebcdic(159) -> 16#FF;
ascii_to_ebcdic(160) -> 16#41;
ascii_to_ebcdic(161) -> 16#AA;
ascii_to_ebcdic(162) -> 16#4A;
ascii_to_ebcdic(163) -> 16#B1;
ascii_to_ebcdic(164) -> 16#9F;
ascii_to_ebcdic(165) -> 16#B2;
ascii_to_ebcdic(166) -> 16#6A;
ascii_to_ebcdic(167) -> 16#B5;
ascii_to_ebcdic(168) -> 16#BB;
ascii_to_ebcdic(169) -> 16#B4;
ascii_to_ebcdic(170) -> 16#9A;
ascii_to_ebcdic(171) -> 16#8A;
ascii_to_ebcdic(172) -> 16#B0;
ascii_to_ebcdic(173) -> 16#CA;
ascii_to_ebcdic(174) -> 16#AF;
ascii_to_ebcdic(175) -> 16#BC;
ascii_to_ebcdic(176) -> 16#90;
ascii_to_ebcdic(177) -> 16#8F;
ascii_to_ebcdic(178) -> 16#EA;
ascii_to_ebcdic(179) -> 16#FA;
ascii_to_ebcdic(180) -> 16#BE;
ascii_to_ebcdic(181) -> 16#A0;
ascii_to_ebcdic(182) -> 16#B6;
ascii_to_ebcdic(183) -> 16#B3;
ascii_to_ebcdic(184) -> 16#9D;
ascii_to_ebcdic(185) -> 16#DA;
ascii_to_ebcdic(186) -> 16#9B;
ascii_to_ebcdic(187) -> 16#8B;
ascii_to_ebcdic(188) -> 16#B7;
ascii_to_ebcdic(189) -> 16#B8;
ascii_to_ebcdic(190) -> 16#B9;
ascii_to_ebcdic(191) -> 16#AB;
ascii_to_ebcdic(192) -> 16#64;
ascii_to_ebcdic(193) -> 16#65;
ascii_to_ebcdic(194) -> 16#62;
ascii_to_ebcdic(195) -> 16#66;
ascii_to_ebcdic(196) -> 16#63;
ascii_to_ebcdic(197) -> 16#67;
ascii_to_ebcdic(198) -> 16#9E;
ascii_to_ebcdic(199) -> 16#68;
ascii_to_ebcdic(200) -> 16#74;
ascii_to_ebcdic(201) -> 16#71;
ascii_to_ebcdic(202) -> 16#72;
ascii_to_ebcdic(203) -> 16#73;
ascii_to_ebcdic(204) -> 16#78;
ascii_to_ebcdic(205) -> 16#75;
ascii_to_ebcdic(206) -> 16#76;
ascii_to_ebcdic(207) -> 16#77;
ascii_to_ebcdic(208) -> 16#AC;
ascii_to_ebcdic(209) -> 16#69;
ascii_to_ebcdic(210) -> 16#ED;
ascii_to_ebcdic(211) -> 16#EE;
ascii_to_ebcdic(212) -> 16#EB;
ascii_to_ebcdic(213) -> 16#EF;
ascii_to_ebcdic(214) -> 16#EC;
ascii_to_ebcdic(215) -> 16#BF;
ascii_to_ebcdic(216) -> 16#80;
ascii_to_ebcdic(217) -> 16#FD;
ascii_to_ebcdic(218) -> 16#FE;
ascii_to_ebcdic(219) -> 16#FB;
ascii_to_ebcdic(220) -> 16#FC;
ascii_to_ebcdic(221) -> 16#BA;
ascii_to_ebcdic(222) -> 16#AE;
ascii_to_ebcdic(223) -> 16#59;
ascii_to_ebcdic(224) -> 16#44;
ascii_to_ebcdic(225) -> 16#45;
ascii_to_ebcdic(226) -> 16#42;
ascii_to_ebcdic(227) -> 16#46;
ascii_to_ebcdic(228) -> 16#43;
ascii_to_ebcdic(229) -> 16#47;
ascii_to_ebcdic(230) -> 16#9C;
ascii_to_ebcdic(231) -> 16#48;
ascii_to_ebcdic(232) -> 16#54;
ascii_to_ebcdic(233) -> 16#51;
ascii_to_ebcdic(234) -> 16#52;
ascii_to_ebcdic(235) -> 16#53;
ascii_to_ebcdic(236) -> 16#58;
ascii_to_ebcdic(237) -> 16#55;
ascii_to_ebcdic(238) -> 16#56;
ascii_to_ebcdic(239) -> 16#57;
ascii_to_ebcdic(240) -> 16#8C;
ascii_to_ebcdic(241) -> 16#49;
ascii_to_ebcdic(242) -> 16#CD;
ascii_to_ebcdic(243) -> 16#CE;
ascii_to_ebcdic(244) -> 16#CB;
ascii_to_ebcdic(245) -> 16#CF;
ascii_to_ebcdic(246) -> 16#CC;
ascii_to_ebcdic(247) -> 16#E1;
ascii_to_ebcdic(248) -> 16#70;
ascii_to_ebcdic(249) -> 16#DD;
ascii_to_ebcdic(250) -> 16#DE;
ascii_to_ebcdic(251) -> 16#DB;
ascii_to_ebcdic(252) -> 16#DC;
ascii_to_ebcdic(253) -> 16#8D;
ascii_to_ebcdic(254) -> 16#8E;
ascii_to_ebcdic(255) -> 16#DF;
ascii_to_ebcdic(L) when is_list(L) ->
	[ascii_to_ebcdic(X) || X <- L];
ascii_to_ebcdic(B) when is_binary(B) ->
	list_to_binary([ascii_to_ebcdic(X) || X <- binary_to_list(B)]).

