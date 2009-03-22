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

%% @doc	Tests for the iso.erl module. Run iso_test:test() to see if everything
%% 		is ok.
%% @end

-module(iso_test).

-compile(export_all).

test() ->
	test_IFA_BITMAP(),
	test_IFB_BITMAP(),
	test_IFA_BINARY(),
	test_IFB_BINARY(),
	test_IFA_LLBINARY(),
	test_IFB_LLBINARY(),
	test_IFA_LLLBINARY(),
	test_IFB_LLLBINARY(),
	test_IFA_NUMERIC(),
	test_IFB_NUMERIC(),
	test_IFA_AMOUNT(),
	test_IFB_AMOUNT(),
	test_IFA_LLNUM(),
	test_IFB_LLNUM(),
	test_IFA_LLLNUM(),
	test_IFB_LLLNUM(),
	test_IF_CHAR(),
	test_IFA_LLCHAR(),
	test_IFA_LLLCHAR(),
	test_IFE_LLLBINARY(),
	test_IFE_NUMERIC(),
	test_IFE_AMOUNT(),
	test_IFE_LLNUM(),
	test_IFE_LLLNUM(),

	test_buildpackager(),
	ok.

test_IFA_BITMAP() ->
	{P,U} = iso:packager_for({0,"name","IFA_BITMAP"}),

	Fields1 = [1,64,127,128],
	<<"80000000000000010000000000000003">> = P(Fields1),
	{Bits1, _} = U(<<"80000000000000010000000000000003">>),
	Fields1 = lists:reverse(Bits1),
	<<"80000000000000010000000000000003">> = P(<<"80000000000000010000000000000003">>),

	Fields2 = [2,3,4,7,11,12,14,22,24,26,32,35,37,41,42,47,49,53,62,64],
	<<"7234054128C28805">> = P(Fields2),
	{Bits2,_} = U(<<"7234054128C28805">>),
	Fields2 = lists:reverse(Bits2),
	<<"7234054128C28805">> = P(<<"7234054128C28805">>),

	Fields3 = [2, 7, 12, 28, 32, 39, 41, 42, 50, 53, 62],
	<<"4210001102C04804">> = P(Fields3),
	{Bits3, _} = U(<<"4210001102C04804">>),
	Fields3 = lists:reverse(Bits3),
	<<"4210001102C04804">> = P(<<"4210001102C04804">>),
	ok.

test_IFB_BITMAP() ->
	{P,U} = iso:packager_for({0,"name","IFB_BITMAP"}),

	Fields1 = [1,64,127,128],
	<<128,0,0,0,0,0,0,1,0,0,0,0,0,0,0,3>> = P(Fields1),
	{Bits1, _} = U(<<128,0,0,0,0,0,0,1,0,0,0,0,0,0,0,3>>),
	Fields1 = lists:reverse(Bits1),
	<<128,0,0,0,0,0,0,1,0,0,0,0,0,0,0,3>> = P(<<128,0,0,0,0,0,0,1,0,0,0,0,0,0,0,3>>),

	Fields2 = [2,3,4,7,11,12,14,22,24,26,32,35,37,41,42,47,49,53,62,64],
	<<114,52,5,65,40,194,136,5>> = P(Fields2),
	{Bits2,_} = U(<<114,52,5,65,40,194,136,5>>),
	Fields2 = lists:reverse(Bits2),
	<<114,52,5,65,40,194,136,5>> = P(<<114,52,5,65,40,194,136,5>>),

	Fields3 = [2, 7, 12, 28, 32, 39, 41, 42, 50, 53, 62],
	<<66,16,0,17,2,192,72,4>> = P(Fields3),
	{Bits3, _} = U(<<66,16,0,17,2,192,72,4>>),
	Fields3 = lists:reverse(Bits3),
	<<66,16,0,17,2,192,72,4>> = P(<<66,16,0,17,2,192,72,4>>),
	ok.

test_IFA_BINARY() ->
	{P,U} = iso:packager_for({8,"name","IFA_BINARY"}),
	{<<16#01,16#23,16#45,16#67,16#89,16#ab,16#cd,16#ef>>,_} = U(<<"0123456789ABCDEF">>),
	<<"0123456789ABCDEF">> = P(<<16#01,16#23,16#45,16#67,16#89,16#ab,16#cd,16#ef>>),
	"0123456789ABCDEF" = P([16#01,16#23,16#45,16#67,16#89,16#ab,16#cd,16#ef]),
	ok.

test_IFB_BINARY() ->
	{P,U} = iso:packager_for({8,"name","IFB_BINARY"}),
	{<<1,35,69,103,137,171,205,239>>,_} = U(<<1,35,69,103,137,171,205,239>>),
	<<1,35,69,103,137,171,205,239>> = P(<<1,35,69,103,137,171,205,239>>),
	[1,35,69,103,137,171,205,239] = P([1,35,69,103,137,171,205,239]),
	ok.

test_IFA_LLBINARY() ->
	{P,U} = iso:packager_for({0,"name","IFA_LLBINARY"}),
	{<<1,35,69,103,137,171,205,239>>,_} = U(<<"160123456789ABCDEF">>),
	["16", <<"0123456789ABCDEF">>] = P(<<1,35,69,103,137,171,205,239>>),
	["16", "0123456789ABCDEF"] = P([1,35,69,103,137,171,205,239]),
	ok.

test_IFB_LLBINARY() ->
	{P,U} = iso:packager_for({0,"name","IFB_LLBINARY"}),
	{<<1,35,69,103,137,171,205,239>>,_} = U(<<$0,$8,1,35,69,103,137,171,205,239>>),
	["08", <<1,35,69,103,137,171,205,239>>] = P(<<1,35,69,103,137,171,205,239>>),
	["08", [1,35,69,103,137,171,205,239]] = P([1,35,69,103,137,171,205,239]),
	ok.

test_IFA_LLLBINARY() ->
	{P,U} = iso:packager_for({0,"name","IFA_LLLBINARY"}),
	{<<1,35,69,103,137,171,205,239>>,_} = U(<<"0160123456789ABCDEF">>),
	["016", <<"0123456789ABCDEF">>] = P(<<1,35,69,103,137,171,205,239>>),
	["016", "0123456789ABCDEF"] = P([1,35,69,103,137,171,205,239]),
	ok.

test_IFB_LLLBINARY() ->
	{P,U} = iso:packager_for({0,"name","IFB_LLLBINARY"}),
	{<<1,35,69,103,137,171,205,239>>,_} = U(<<$0,$0,$8,1,35,69,103,137,171,205,239>>),
	["008", <<1,35,69,103,137,171,205,239>>] = P(<<1,35,69,103,137,171,205,239>>),
	["008", [1,35,69,103,137,171,205,239]] = P([1,35,69,103,137,171,205,239]),
	ok.

test_IFA_NUMERIC() ->
	{P,U} = iso:packager_for({10,"name","IFA_NUMERIC"}),
	{<<"0123456789">>,_} = U(<<"0123456789">>),
	<<"0123456789">> = P(<<"0123456789">>),
	"0123456789" = P("0123456789"),
	"0123456789" = P(123456789),
	ok.

test_IFB_NUMERIC() ->
	{P,U} = iso:packager_for({10,"name","IFB_NUMERIC"}),
	{<<"0123456789">>,_} = U(<<1,35,69,103,137>>),
	<<1,35,69,103,137>> = P(<<"0123456789">>),
	<<1,35,69,103,137>> = P("0123456789"),
	<<1,35,69,103,137>> = P(123456789),
	ok.

test_IFA_AMOUNT() ->
	{P,U} = iso:packager_for({11,"name","IFA_AMOUNT"}),
	{<<"C0123456789">>,_} = U(<<"C0123456789">>),
	<<"C0123456789">> = P(<<"C0123456789">>),
	"C0123456789" = P("C0123456789"),
	"C0123456789" = P(123456789),

	{<<"D0123456789">>,_} = U(<<"D0123456789">>),
	<<"D0123456789">> = P(<<"D0123456789">>),
	"D0123456789" = P("D0123456789"),
	"D0123456789" = P(-123456789),
	ok.

test_IFB_AMOUNT() ->
	{P,U} = iso:packager_for({11,"name","IFB_AMOUNT"}),
	{<<"C0123456789">>,_} = U(<<192,18,52,86,120,144>>),
	<<192,18,52,86,120,144>> = P(<<"C0123456789">>),
	<<192,18,52,86,120,144>> = P("C0123456789"),
	<<192,18,52,86,120,144>> = P(123456789),

	{<<"D0123456789">>,_} = U(<<208,18,52,86,120,144>>),
	<<208,18,52,86,120,144>> = P(<<"D0123456789">>),
	<<208,18,52,86,120,144>> = P("D0123456789"),
	<<208,18,52,86,120,144>> = P(-123456789),
	ok.

test_IFA_LLNUM() ->
	{P,U} = iso:packager_for({0,"name","IFA_LLNUM"}),
	{<<"0123456789">>,_} = U(<<"100123456789">>),
	["10", <<"0123456789">>] = P(<<"0123456789">>),
	["10", "0123456789"] = P("0123456789"),
	["09", "123456789"] = P(123456789),
	ok.

test_IFB_LLNUM() ->
	{P,U} = iso:packager_for({0,"name","IFB_LLNUM"}),
	{<<"0123456789">>,_} = U(<<$1,$0,1,35,69,103,137>>),
	["10", <<1,35,69,103,137>>] = P(<<"0123456789">>),
	["10", <<1,35,69,103,137>>] = P("0123456789"),
	["09", <<18,52,86,120,144>>] = P(123456789),
	ok.

test_IFA_LLLNUM() ->
	{P,U} = iso:packager_for({0,"name","IFA_LLLNUM"}),
	{<<"0123456789">>,_} = U(<<"0100123456789">>),
	["010", <<"0123456789">>] = P(<<"0123456789">>),
	["010", "0123456789"] = P("0123456789"),
	["009", "123456789"] = P(123456789),
	ok.

test_IFB_LLLNUM() ->
	{P,U} = iso:packager_for({0,"name","IFB_LLLNUM"}),
	{<<"0123456789">>,_} = U(<<$0,$1,$0,1,35,69,103,137>>),
	["010", <<1,35,69,103,137>>] = P(<<"0123456789">>),
	["010", <<1,35,69,103,137>>] = P("0123456789"),
	["009", <<18,52,86,120,144>>] = P(123456789),
	ok.

test_IF_CHAR() ->
	{P,U} = iso:packager_for({10,"name","IF_CHAR"}),
	{<<"HELLO!    ">>,_} = U(<<"HELLO!    ">>),
	<<"HELLO!    ">> = P(<<"HELLO!    ">>),
	"HELLO!    " = P("HELLO!"),
	ok.

test_IFA_LLCHAR() ->
	{P,U} = iso:packager_for({0,"name","IFA_LLCHAR"}),
	{<<"HELLO!    ">>,_} = U(<<"10HELLO!    ">>),
	["10", <<"HELLO!    ">>] = P(<<"HELLO!    ">>),
	["10", "HELLO!    "] = P("HELLO!    "),
	["06", "HELLO!"] = P("HELLO!"),
	ok.

test_IFA_LLLCHAR() ->
	{P,U} = iso:packager_for({0,"name","IFA_LLLCHAR"}),
	{<<"HELLO!    ">>,_} = U(<<"010HELLO!    ">>),
	["010", <<"HELLO!    ">>] = P(<<"HELLO!    ">>),
	["010", "HELLO!    "] = P("HELLO!    "),
	["006", "HELLO!"] = P("HELLO!"),
	ok.

test_IFE_LLLBINARY() ->
	{P,U} = iso:packager_for({0,"name","IFE_LLLBINARY"}),
	{<<1,35,69,103,137,171,205,239>>,_} = U(<<16#f0,16#f0,16#f8,1,35,69,103,137,171,205,239>>),
	[[16#f0,16#f0,16#f8], <<1,35,69,103,137,171,205,239>>] = P(<<1,35,69,103,137,171,205,239>>),
	[[16#f0,16#f0,16#f8], [1,35,69,103,137,171,205,239]] = P([1,35,69,103,137,171,205,239]),
	ok.

test_IFE_NUMERIC() ->
	{P,U} = iso:packager_for({10,"name","IFE_NUMERIC"}),
	{<<"0123456789">>,_} = U(<<16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9>>),
	<<16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9>> = P(<<"0123456789">>),
	[16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9] = P("0123456789"),
	[16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9] = P(123456789),
	ok.

test_IFE_AMOUNT() ->
	{P,U} = iso:packager_for({11,"name","IFE_AMOUNT"}),
	{<<"C0123456789">>,_} = U(<<16#c3,16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9>>),
	<<16#c3,16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9>> = P(<<"C0123456789">>),
	[16#c3,16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9] = P("C0123456789"),
	[16#c3,16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9] = P(123456789),

	{<<"D0123456789">>,_} = U(<<16#c4,16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9>>),
	<<16#c4,16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9>> = P(<<"D0123456789">>),
	[16#c4,16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9] = P("D0123456789"),
	[16#c4,16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9] = P(-123456789),
	ok.

test_IFE_LLNUM() ->
	{P,U} = iso:packager_for({0,"name","IFE_LLNUM"}),
	{<<"0123456789">>,_} = U(<<16#f1,16#f0,16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9>>),
	[[16#f1,16#f0], <<16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9>>] = P(<<"0123456789">>),
	[[16#f1,16#f0], [16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9]] = P("0123456789"),
	[[16#f0,16#f9], [16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9]] = P(123456789),
	ok.

test_IFE_LLLNUM() ->
	{P,U} = iso:packager_for({0,"name","IFE_LLLNUM"}),
	{<<"0123456789">>,_} = U(<<16#f0,16#f1,16#f0,16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9>>),
	[[16#f0,16#f1,16#f0], <<16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9>>] = P(<<"0123456789">>),
	[[16#f0,16#f1,16#f0], [16#f0,16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9]] = P("0123456789"),
	[[16#f0,16#f0,16#f9], [16#f1,16#f2,16#f3,16#f4,16#f5,16#f6,16#f7,16#f8,16#f9]] = P(123456789),
	ok.



%% add more tests above this line

test_buildpackager() ->
	{Xml, _} = xmerl_scan:string(iso_spec()),
	P = iso:build_packager(Xml),
	M = [
		{0, "0100"},
		{4, 1234},
		{11, 987654},
		{12, iso_util:to_list({19,58,03})},
		{13, iso_util:to_list({2009,1,1})},
		{38, "TESTOK"}
	],
	<<"010010380000040000000000000012349876541958030101TESTOK">> = iso:pack(P, M),
	M1 = iso:unpack(P, <<"010010380000040000000000000012349876541958030101TESTOK">>),
	<<"0100">> = iso:get_MTI(M1),
	<<"000000001234">> = iso:get_field(4, M1),
	<<"987654">> = iso:get_field(11, M1),
	<<"195803">> = iso:get_field(12, M1),
	<<"0101">> = iso:get_field(13, M1),
	<<"TESTOK">> = iso:get_field(38, M1),
	ok.

iso_spec() ->
	%% jpos packager from base24.xml file
	"<!-- BASE24 field descriptions for GenericPackager -->

<isopackager>
  <isofield
      id=\"0\"
      length=\"4\"
      name=\"MESSAGE TYPE INDICATOR\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"1\"
      length=\"16\"
      name=\"BIT MAP\"
      class=\"org.jpos.iso.IFA_BITMAP\"/>
  <isofield
      id=\"2\"
      length=\"19\"
      name=\"PAN - PRIMARY ACCOUNT NUMBER\"
      class=\"org.jpos.iso.IFA_LLNUM\"/>
  <isofield
      id=\"3\"
      length=\"6\"
      name=\"PROCESSING CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"4\"
      length=\"12\"
      name=\"AMOUNT, TRANSACTION\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"5\"
      length=\"12\"
      name=\"AMOUNT, SETTLEMENT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"6\"
      length=\"12\"
      name=\"AMOUNT, CARDHOLDER BILLING\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"7\"
      length=\"10\"
      name=\"TRANSMISSION DATE AND TIME\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"8\"
      length=\"8\"
      name=\"AMOUNT, CARDHOLDER BILLING FEE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"9\"
      length=\"8\"
      name=\"CONVERSION RATE, SETTLEMENT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"10\"
      length=\"8\"
      name=\"CONVERSION RATE, CARDHOLDER BILLING\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"11\"
      length=\"6\"
      name=\"SYSTEM TRACE AUDIT NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"12\"
      length=\"6\"
      name=\"TIME, LOCAL TRANSACTION\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"13\"
      length=\"4\"
      name=\"DATE, LOCAL TRANSACTION\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"14\"
      length=\"4\"
      name=\"DATE, EXPIRATION\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"15\"
      length=\"4\"
      name=\"DATE, SETTLEMENT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"16\"
      length=\"4\"
      name=\"DATE, CONVERSION\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"17\"
      length=\"4\"
      name=\"DATE, CAPTURE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"18\"
      length=\"4\"
      name=\"MERCHANTS TYPE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"19\"
      length=\"3\"
      name=\"ACQUIRING INSTITUTION COUNTRY CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"20\"
      length=\"3\"
      name=\"PAN EXTENDED COUNTRY CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"21\"
      length=\"3\"
      name=\"FORWARDING INSTITUTION COUNTRY CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"22\"
      length=\"3\"
      name=\"POINT OF SERVICE ENTRY MODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"23\"
      length=\"3\"
      name=\"CARD SEQUENCE NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"24\"
      length=\"3\"
      name=\"NETWORK INTERNATIONAL IDENTIFIEER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"25\"
      length=\"2\"
      name=\"POINT OF SERVICE CONDITION CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"26\"
      length=\"2\"
      name=\"POINT OF SERVICE PIN CAPTURE CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"27\"
      length=\"1\"
      name=\"AUTHORIZATION IDENTIFICATION RESP LEN\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"28\"
      length=\"9\"
      name=\"AMOUNT, TRANSACTION FEE\"
      class=\"org.jpos.iso.IFA_AMOUNT\"/>
  <isofield
      id=\"29\"
      length=\"9\"
      name=\"AMOUNT, SETTLEMENT FEE\"
      class=\"org.jpos.iso.IFA_AMOUNT\"/>
  <isofield
      id=\"30\"
      length=\"9\"
      name=\"AMOUNT, TRANSACTION PROCESSING FEE\"
      class=\"org.jpos.iso.IFA_AMOUNT\"/>
  <isofield
      id=\"31\"
      length=\"9\"
      name=\"AMOUNT, SETTLEMENT PROCESSING FEE\"
      class=\"org.jpos.iso.IFA_AMOUNT\"/>
  <isofield
      id=\"32\"
      length=\"11\"
      name=\"ACQUIRING INSTITUTION IDENT CODE\"
      class=\"org.jpos.iso.IFA_LLNUM\"/>
  <isofield
      id=\"33\"
      length=\"11\"
      name=\"FORWARDING INSTITUTION IDENT CODE\"
      class=\"org.jpos.iso.IFA_LLNUM\"/>
  <isofield
      id=\"34\"
      length=\"28\"
      name=\"PAN EXTENDED\"
      class=\"org.jpos.iso.IFA_LLCHAR\"/>
  <isofield
      id=\"35\"
      length=\"37\"
      name=\"TRACK 2 DATA\"
      class=\"org.jpos.iso.IFA_LLNUM\"/>
  <isofield
      id=\"36\"
      length=\"104\"
      name=\"TRACK 3 DATA\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"37\"
      length=\"12\"
      name=\"RETRIEVAL REFERENCE NUMBER\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"38\"
      length=\"6\"
      name=\"AUTHORIZATION IDENTIFICATION RESPONSE\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"39\"
      length=\"2\"
      name=\"RESPONSE CODE\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"40\"
      length=\"3\"
      name=\"SERVICE RESTRICTION CODE\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"41\"
      length=\"16\"
      name=\"CARD ACCEPTOR TERMINAL IDENTIFICACION\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"42\"
      length=\"15\"
      name=\"CARD ACCEPTOR IDENTIFICATION CODE\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"43\"
      length=\"40\"
      name=\"CARD ACCEPTOR NAME/LOCATION\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"44\"
      length=\"25\"
      name=\"ADITIONAL RESPONSE DATA\"
      class=\"org.jpos.iso.IFA_LLCHAR\"/>
  <isofield
      id=\"45\"
      length=\"76\"
      name=\"TRACK 1 DATA\"
      class=\"org.jpos.iso.IFA_LLCHAR\"/>
  <isofield
      id=\"46\"
      length=\"999\"
      name=\"ADITIONAL DATA - ISO\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"47\"
      length=\"999\"
      name=\"ADITIONAL DATA - NATIONAL\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"48\"
      length=\"999\"
      name=\"ADITIONAL DATA - PRIVATE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"49\"
      length=\"3\"
      name=\"CURRENCY CODE, TRANSACTION\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"50\"
      length=\"3\"
      name=\"CURRENCY CODE, SETTLEMENT\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"51\"
      length=\"3\"
      name=\"CURRENCY CODE, CARDHOLDER BILLING\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"52\"
      length=\"8\"
      name=\"PIN DATA\"
      class=\"org.jpos.iso.IFA_BINARY\"/>
  <isofield
      id=\"53\"
      length=\"16\"
      name=\"SECURITY RELATED CONTROL INFORMATION\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"54\"
      length=\"120\"
      name=\"ADDITIONAL AMOUNTS\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"55\"
      length=\"999\"
      name=\"RESERVED ISO\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"56\"
      length=\"999\"
      name=\"RESERVED ISO\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"57\"
      length=\"999\"
      name=\"RESERVED NATIONAL\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"58\"
      length=\"999\"
      name=\"RESERVED NATIONAL\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"59\"
      length=\"999\"
      name=\"RESERVED NATIONAL\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"60\"
      length=\"999\"
      name=\"RESERVED PRIVATE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"61\"
      length=\"999\"
      name=\"RESERVED PRIVATE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"62\"
      length=\"999\"
      name=\"RESERVED PRIVATE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"63\"
      length=\"999\"
      name=\"RESERVED PRIVATE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"64\"
      length=\"16\"
      name=\"MESSAGE AUTHENTICATION CODE FIELD\"
      class=\"org.jpos.iso.IFA_BINARY\"/>
  <isofield
      id=\"65\"
      length=\"1\"
      name=\"BITMAP, EXTENDED\"
      class=\"org.jpos.iso.IFA_BINARY\"/>
  <isofield
      id=\"66\"
      length=\"1\"
      name=\"SETTLEMENT CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"67\"
      length=\"2\"
      name=\"EXTENDED PAYMENT CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"68\"
      length=\"3\"
      name=\"RECEIVING INSTITUTION COUNTRY CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"69\"
      length=\"3\"
      name=\"SETTLEMENT INSTITUTION COUNTRY CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"70\"
      length=\"3\"
      name=\"NETWORK MANAGEMENT INFORMATION CODE\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"71\"
      length=\"4\"
      name=\"MESSAGE NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"72\"
      length=\"4\"
      name=\"MESSAGE NUMBER LAST\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"73\"
      length=\"6\"
      name=\"DATE ACTION\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"74\"
      length=\"10\"
      name=\"CREDITS NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"75\"
      length=\"10\"
      name=\"CREDITS REVERSAL NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"76\"
      length=\"10\"
      name=\"DEBITS NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"77\"
      length=\"10\"
      name=\"DEBITS REVERSAL NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"78\"
      length=\"10\"
      name=\"TRANSFER NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"79\"
      length=\"10\"
      name=\"TRANSFER REVERSAL NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"80\"
      length=\"10\"
      name=\"INQUIRIES NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"81\"
      length=\"10\"
      name=\"AUTHORIZATION NUMBER\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"82\"
      length=\"12\"
      name=\"CREDITS, PROCESSING FEE AMOUNT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"83\"
      length=\"12\"
      name=\"CREDITS, TRANSACTION FEE AMOUNT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"84\"
      length=\"12\"
      name=\"DEBITS, PROCESSING FEE AMOUNT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"85\"
      length=\"12\"
      name=\"DEBITS, TRANSACTION FEE AMOUNT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"86\"
      length=\"16\"
      name=\"CREDITS, AMOUNT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"87\"
      length=\"16\"
      name=\"CREDITS, REVERSAL AMOUNT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"88\"
      length=\"16\"
      name=\"DEBITS, AMOUNT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"89\"
      length=\"16\"
      name=\"DEBITS, REVERSAL AMOUNT\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"90\"
      length=\"42\"
      name=\"ORIGINAL DATA ELEMENTS\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
  <isofield
      id=\"91\"
      length=\"1\"
      name=\"FILE UPDATE CODE\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"92\"
      length=\"2\"
      name=\"FILE SECURITY CODE\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"93\"
      length=\"6\"
      name=\"RESPONSE INDICATOR\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"94\"
      length=\"7\"
      name=\"SERVICE INDICATOR\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"95\"
      length=\"42\"
      name=\"REPLACEMENT AMOUNTS\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"96\"
      length=\"16\"
      name=\"MESSAGE SECURITY CODE\"
      class=\"org.jpos.iso.IFA_BINARY\"/>
  <isofield
      id=\"97\"
      length=\"17\"
      name=\"AMOUNT, NET SETTLEMENT\"
      class=\"org.jpos.iso.IFA_AMOUNT\"/>
  <isofield
      id=\"98\"
      length=\"25\"
      name=\"PAYEE\"
      class=\"org.jpos.iso.IF_CHAR\"/>
  <isofield
      id=\"99\"
      length=\"11\"
      name=\"SETTLEMENT INSTITUTION IDENT CODE\"
      class=\"org.jpos.iso.IFA_LLNUM\"/>
  <isofield
      id=\"100\"
      length=\"11\"
      name=\"RECEIVING INSTITUTION IDENT CODE\"
      class=\"org.jpos.iso.IFA_LLNUM\"/>
  <isofield
      id=\"101\"
      length=\"17\"
      name=\"FILE NAME\"
      class=\"org.jpos.iso.IFA_LLCHAR\"/>
  <isofield
      id=\"102\"
      length=\"28\"
      name=\"ACCOUNT IDENTIFICATION 1\"
      class=\"org.jpos.iso.IFA_LLCHAR\"/>
  <isofield
      id=\"103\"
      length=\"28\"
      name=\"ACCOUNT IDENTIFICATION 2\"
      class=\"org.jpos.iso.IFA_LLCHAR\"/>
  <isofield
      id=\"104\"
      length=\"100\"
      name=\"TRANSACTION DESCRIPTION\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"105\"
      length=\"999\"
      name=\"RESERVED ISO USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"106\"
      length=\"999\"
      name=\"RESERVED ISO USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"107\"
      length=\"999\"
      name=\"RESERVED ISO USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"108\"
      length=\"999\"
      name=\"RESERVED ISO USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"109\"
      length=\"999\"
      name=\"RESERVED ISO USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"110\"
      length=\"999\"
      name=\"RESERVED ISO USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"111\"
      length=\"999\"
      name=\"RESERVED ISO USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"112\"
      length=\"999\"
      name=\"RESERVED NATIONAL USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"113\"
      length=\"999\"
      name=\"RESERVED NATIONAL USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"114\"
      length=\"999\"
      name=\"RESERVED NATIONAL USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"115\"
      length=\"999\"
      name=\"RESERVED NATIONAL USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"116\"
      length=\"999\"
      name=\"RESERVED NATIONAL USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"117\"
      length=\"999\"
      name=\"RESERVED NATIONAL USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"118\"
      length=\"999\"
      name=\"RESERVED NATIONAL USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"119\"
      length=\"999\"
      name=\"RESERVED NATIONAL USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"120\"
      length=\"999\"
      name=\"RESERVED PRIVATE USE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"121\"
      length=\"23\"
      name=\"S-121 BASE24-POS AUTH INDICATORS\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"122\"
      length=\"14\"
      name=\"S-122 CARD ISSUER IDENTIFICATION CODE\"
      class=\"org.jpos.iso.IFA_LLLNUM\"/>
  <isofield
      id=\"123\"
      length=\"153\"
      name=\"S-123 CRYPTOGRAFIC SERVICE MESSAGE\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"124\"
      length=\"12\"
      name=\"S-124 DEPOSIT TYPE OR BATCH/SHIFT DATA\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"125\"
      length=\"15\"
      name=\"S-125 ATM ACCT INDICATOR OR POS SETTLEMENT DATA\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"126\"
      length=\"999\"
      name=\"S-126 ATM ADDIC. DATA OR POS PRE-AUTH/CHARGEBACK\"
      class=\"org.jpos.iso.IFA_LLLCHAR\"/>
  <isofield
      id=\"127\"
      length=\"99\"
      name=\"BASE24-POS User Data\"
      class=\"org.jpos.iso.IFA_LLCHAR\"/>
  <isofield
      id=\"128\"
      length=\"16\"
      name=\"MAC 2\"
      class=\"org.jpos.iso.IFA_NUMERIC\"/>
</isopackager>".

