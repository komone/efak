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

-module(ts_config_iso8583).

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_iso8583.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);

parse_config(Element = #xmlElement{name=iso8583},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar=DynVar,
			    subst    = SubstFlag, match=MatchRegExp}) ->

    Packager = ts_config:get_default(Tab, iso8583_packager, nil),
    SizeHeader = ts_config:get_default(Tab, iso8583_size_header, nil),
	
	Fields = lists:foldl(fun parse_iso_fields/2, [], Element#xmlElement.content),

    Msg= #ts_request{ack     = parse,
                     endpage = true,
                     dynvar_specs  = DynVar,
                     subst   = SubstFlag,
                     match   = MatchRegExp,
                     param   = #iso8583_request{
												packager = Packager,
												size_header = SizeHeader,
												fields = Fields
												}
					 },

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id}, Msg }),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=undefined},
                 Element#xmlElement.content);

%% Parsing options
parse_config(Element = #xmlElement{name=option}, Conf = #config{session_tab = Tab}) ->
    case ts_config:getAttr(Element#xmlElement.attributes, name) of
        "isopackager" ->
            FileName = ts_config:getAttr(string,Element#xmlElement.attributes, value,"isopackager.xml"),
			Packager = iso:build_packager(FileName),
            ets:insert(Tab,{{iso8583_packager,value}, Packager});
        "size_header" ->
            SizeH = ts_config:getAttr(string,Element#xmlElement.attributes, value, "display:0"),
			[FormatString, SizeString] = string:tokens(SizeH, ":"),
			Format =
            case FormatString of
				"big-endian" -> 
					{big_endian, list_to_integer(SizeString)*8};
				"little-endian" ->
					{little_endian, list_to_integer(SizeString)*8};
				"display" ->
					{display, list_to_integer(SizeString)}
			end,
            ets:insert(Tab,{{iso8583_size_header,value}, Format})
    end,
    lists:foldl( fun(A,B) -> ts_config:parse(A,B) end, Conf, Element#xmlElement.content);

%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);

%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

parse_iso_fields(Element = #xmlElement{name=field}, Fields) ->
	FieldId = ts_config:getAttr(integer,Element#xmlElement.attributes, id, 0),
    FieldVal = ts_config:getAttr(string,Element#xmlElement.attributes, value, ""),
	[{FieldId, FieldVal} | Fields];

parse_iso_fields(_Element, Fields) ->
	Fields.

