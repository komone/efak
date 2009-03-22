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

%% @doc	Starts the interface given a configured name.
%%
%%		The interface confugration is expected to follow the structure below
%%
%%```
%%<interface
%%	name="Name",
%%	iso_version="V",
%%	isopackager="IsoPackager",
%%	pool_size="Number",
%%	callback="{M,F,A}",
%%  security_module="sec_mod"
%%	status="Status",
%%	sign_on="Bool",
%%	local_inst_id="IdOrAny",
%%	remote_inst_id=IdOrAny,
%%	cut_off="HH:MM:SS",
%%	currency="Code",
%%	retry="Retry",
%%	repeat_timeout="Msecs">
%%	<msg_composition>
%%		<msg mti="XXXX">
%%			<field presence="F">Id</field>
%%		</msg>
%%	</msg_composition>
%%	<nodes>
%%		<node>onenode@onehost</node>
%%		<node>othernode@otherhost</node>
%%	</nodes>
%%</interface>
%%'''
%%
%% 		Allowed configuration parameters are:
%%```
%%V				"0" = ISO8583v87 | "1" = ISO8583v93 | "2" = ISO8583v03
%%IsoPackager	name of the isopackager used to (un)pack messages
%%Number		number of workers used to (un)pack messages
%%{M,F,A}		module,function,args to send messages to application
%%sec_mod		module that exports calc_mac/1 function
%%Status		up | down status of interface on startup
%%Bool			true | false can reply to login/logoff messages
%%IdOrAny		string | any if any no check is done
%%HH:MM:SS		time
%%Code			number that identify reconciliation currency
%%Retry			number of max retry when sending notification
%%Msecs 		max milliseconds to wait notification reply
%%F				m | o indicates field is mandatory or optional
%%Id 			number of iso8583 field
%%			
%%msg_composition contains the message specs for message type identifier XXXX,
%%the specs are composed of field tags indicating presence.'''
%% @end

-module(iso_interface_sup).

-behaviour(supervisor).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1]).

%%--------------------------------------------------------------------
%% Internal exports - supervisor callbacks
%%--------------------------------------------------------------------
-export([init/1]).

-include("interface_conf.hrl").

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Name) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Name 	= atom()
%%				Pid 	= pid()
%%
%% @doc		Starts the interface supervisor for interface named Name.
%%			The process is locally registered as `NAME_sup' because there could be more interface supervisors in one node.
%% @end
%%--------------------------------------------------------------------
start_link(Name) when is_atom(Name) ->
	SupName = list_to_atom(lists:flatten([atom_to_list(Name), "_sup"])),
	supervisor:start_link({local, SupName}, ?MODULE, Name).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Args) ->
%%				{ok,  {SupFlags,  [ChildSpec]}}	|
%%				ignore							|
%%				{error, Reason}
%%
%% @doc     Whenever a supervisor is started using 
%%			supervisor:start_link/[2,3], this function is called by
%%			the new process to find out about restart strategy,
%%			maximum restart frequency and child specifications.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(Name) ->
	case load_config(Name) of
		[] -> ignore;
		[{iso_interface, InterfaceAttr, Content}] ->
			Conf = load_interface_record(InterfaceAttr, Content),
			{ok,
				{{one_for_one, 3, 5},
				[
					{handler_sup, {iso_handler_sup, start_link, [Conf]},
						permanent, infinity, supervisor, [handler_sup]},
					{controller, {iso_interface, start_cluster, [Conf]},
						permanent, brutal_kill, worker, [iso_interface]},

					%% the saf has to be the last one in the activation
					%% order because it is responsible for registration
					%% within the line_proxy server, notifying the
					%% successful initialization of the interface.
					{saf, {iso_saf, start_link, [Conf]},
						permanent, brutal_kill, worker, [iso_saf]}
				]
				}
			}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Name) -> SimpleXML
%%
%%				Name 		= atom()
%%				SimpleXML	= list(term())
%%
%% @doc     Returns the simple_xml confugration for the interface named Name.
%% @hidden
%% @end
%%--------------------------------------------------------------------
load_config(Name) ->
	Q = lists:flatten(["//iso_interface[nodes/node[.='", atom_to_list(node()),"']]"]),
	AllInterfaces = config_server:to_simple_xml(config_server:query_param(Q)),
	NameAsString = atom_to_list(Name),
	lists:filter(fun({iso_interface,Attr,_}) ->
					config_server:get_attr_value(name,Attr) =:= NameAsString end,
					AllInterfaces).

%%--------------------------------------------------------------------
%% @spec	(Name, Contents) -> Element
%%
%%				Name		= atom()
%%				Contents	= [Element]
%%				Element 	= {Name, Attrs, Contents}
%%
%% @doc     Search for a simple_xml element from a Contents list.
%% @hidden
%% @end
%%--------------------------------------------------------------------
get_element(Name, Contents) ->
	{value, Element} = lists:keysearch(Name, 1, Contents),
	Element.

%%--------------------------------------------------------------------
%% @spec	(AttrList, Content) -> #interface_conf{}
%%
%%				AttrList	= [Attr]
%%				Attr 		= {Name, Value}
%%				Contents	= list(term())
%%
%% @doc     Initialize the #interface_conf{} record.
%% @hidden
%% @end
%%--------------------------------------------------------------------
load_interface_record(Attrs, Contents) ->
	[H,M,S] = string:tokens(config_server:get_attr_value(cut_off, Attrs),":"),
	{ok, Tokens, _} = erl_scan:string(config_server:get_attr_value(callback, Attrs) ++ "."),
	{ok, Callback} = erl_parse:parse_term(Tokens),
	#interface_conf{
		name = list_to_atom(config_server:get_attr_value(name, Attrs)),
		iso_version = case config_server:get_attr_value(iso_version, Attrs) of
							"0" -> $0;
							"1" -> $1;
							"2" -> $2
						end,
		validator = get_validator(Contents),
		packer = get_packager(config_server:get_attr_value(isopackager, Attrs)),
		pool_size = list_to_integer(config_server:get_attr_value(pool_size, Attrs)),
		callback = Callback,
		security_module = list_to_atom(config_server:get_attr_value(security_module, Attrs)),
		status = list_to_atom(config_server:get_attr_value(status, Attrs)),
		sign_on = list_to_atom(config_server:get_attr_value(sign_on, Attrs)),
		local_inst_id =	case config_server:get_attr_value(local_inst_id, Attrs) of
						    "any" -> any;
							String -> String
						end,
		remote_inst_id = case config_server:get_attr_value(remote_inst_id, Attrs) of
						    "any" -> any;
							String -> String
						end,
		cut_off = {list_to_integer(H),list_to_integer(M),list_to_integer(S)},
		currency = list_to_integer(config_server:get_attr_value(currency, Attrs)),
		timeout = list_to_integer(config_server:get_attr_value(timeout, Attrs)),
		retry = list_to_integer(config_server:get_attr_value(retry, Attrs)),
		repeat_timeout = list_to_integer(config_server:get_attr_value(repeat_timeout, Attrs))
		%% current_date, -- used internally by iso_handler
	}.

%%--------------------------------------------------------------------
%% @spec	(Content) -> [{MTI, Mandatory, Optional}]
%%
%%				Content 	= list(term())
%%				MTI			= string()
%%				Mandatory	= [int()]
%%				Optional	= [int()]
%%
%% @doc     Returns message specs for message MTI.
%% @hidden
%% @end
%%--------------------------------------------------------------------
get_validator(Content) ->
	{_,_,MsgList} = get_element(msg_composition, Content),
	lists:map(fun({msg, Attr, Fields}) ->
				{Mandatory, Optional} =
				lists:foldl(fun 
								({field, [{presence,"m"}], [Id]}, {M,O}) ->
									{[list_to_integer(Id) | M],O};
								({field, [{presence,"o"}], [Id]}, {M,O}) ->
									{M, [list_to_integer(Id) | O]}
							end, {[],[]}, Fields),
				{config_server:get_attr_value(mti, Attr), [0 | Mandatory], Optional}
			end, MsgList).

%%--------------------------------------------------------------------
%% @spec	(Name) -> {Packer, Unpacker}
%%
%%				Name 		= string()
%%				Packer		= dict()
%%				Unpacker	= dict()
%%
%% @doc     Returns the iso packager named Name.
%% @hidden
%% @end
%%--------------------------------------------------------------------
get_packager(Name) ->
	XPathQuery = lists:flatten(["//isopackager[@name=\"", Name, "\"]"]),
	[XML] = config_server:query_param(XPathQuery),
	iso:build_packager(XML).

