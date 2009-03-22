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

%% @doc	Starts the erl_interface.
%%
%%		The erl_interface lets erlang programs to start transactions
%% 		in an application that uses efak.
%% 
%%		The interface configration is expected to follow the structure below
%%
%%```
%%<erl_interface
%%	name="Name"
%%	callback="{M,F,A}"
%%	<nodes>
%%		<node>onenode@onehost</node>
%%		<node>othernode@otherhost</node>
%%	</nodes>
%%</erl_interface>
%%'''
%%
%% 		Name is used to identify messages coming from the
%% 		erl_interface process.
%% 		The callback triplet is be used to deliver messages.
%%
%% @see erl_interface
%% @end

-module(erl_interface_sup).

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
%% @end
%%--------------------------------------------------------------------
start_link(Name) when is_atom(Name) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Name).

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
		[{erl_interface, Attrs, _}] ->
			{ok, Tokens, _} = erl_scan:string(get_attr_value(callback, Attrs) ++ "."),
			{ok, Callback} = erl_parse:parse_term(Tokens),
			{ok,
				{{one_for_one, 3, 5},
				[
					{erl_interface, {erl_interface, start_link, [Name, Callback]},
						permanent, brutal_kill, worker, [erl_interface]}
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
	Q = lists:flatten(["//erl_interface[nodes/node[.='", atom_to_list(node()),"']]"]),
	AllInterfaces = config_server:to_simple_xml(config_server:query_param(Q)),
	NameAsString = atom_to_list(Name),
	lists:filter(fun({erl_interface,Attr,_}) ->
					get_attr_value(name,Attr) =:= NameAsString end,
					AllInterfaces).

%%--------------------------------------------------------------------
%% @spec	(Name, AttrList) -> Value
%%
%%				Name		= atom()
%%				AttrList	= [Attr]
%%				Attr 		= {Name, Value}
%%
%% @doc     Search for a simple_xml attribute value from an attribute list.
%% @hidden
%% @end
%%--------------------------------------------------------------------
get_attr_value(Name, AttrList) ->
	{value, {_, Value}} = lists:keysearch(Name, 1, AttrList),
	Value.

