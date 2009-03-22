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

%% @doc	This supervisor starts the interfaces configured for the current node.
%%		If no interface has to be started, it returns `ignore' in the init() function.
%%
%%		{@link config_server} is used to retrieve interface configuration.
%% @end

-module(interfaces_sup).

-behaviour(supervisor).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0]).

%%--------------------------------------------------------------------
%% Internal exports - supervisor callbacks
%%--------------------------------------------------------------------
-export([init/1]).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	() -> {ok, Pid} | ignore | {error, Error}
%%
%%				Pid = pid()
%%
%% @doc		Starts the supervisor.
%%			ChildSpecs are built according to configuration provided by `config_server'.
%% @end	
%%--------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Args) ->
%%				{ok,  {SupFlags,  [ChildSpec]}}	|
%%				ignore							|
%%				{error, Reason}
%%
%% @doc     Starts the interface supervisors for the current node if any.
%%			Max 3 start retries are allowed within 5 seconds.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([]) ->
	case load_config() of
		[] -> ignore;
		InterfaceNames ->
			Services =	{svc,
						{interface_svc_sup, start_link, []},
						permanent,
						infinity,
						supervisor,
						[interface_svc_sup]},
			Interfaces = lists:map(fun interface_spec_for/1, InterfaceNames),

			%% Start timeout has to be synchronized with interfaces_svc_sup MNESIA_WAIT_TIMEOUT
			{ok, {{one_for_one, 3, 60}, [Services | Interfaces]}}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	() -> [Interface]
%%
%%				Interface 	= atom()
%%
%% @doc     Returns the interface names to be started on the current node.
%% @hidden
%% @end
%%--------------------------------------------------------------------
load_config() ->
	Q = lists:flatten(["//interfaces/*[nodes/node[.='", atom_to_list(node()),"']]"]),
	Interfaces = config_server:to_simple_xml(config_server:query_param(Q)),
	lists:foldl(fun ({Interface, Attr, _}, Names) ->
					[{Interface, list_to_atom(get_attr_value(name, Attr))} | Names]
				end, [], Interfaces).

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

%%--------------------------------------------------------------------
%% @spec	({Interface, Name}) -> InterfaceSpec
%%
%%				Interface		= atom()
%%				Name			= atom()
%% 				InterfaceSpec	= {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% 				Id 				= term()
%%				StartFunc		= {atom(),atom(),term()}
%% 				A 				= [term()]
%%				Restart 		= permanent | transient | temporary
%%				Shutdown 		= brutal_kill | int() | infinity
%% 				Type 			= worker | supervisor
%% 				Modules 		= [Module] | dynamic
%%  			Module 			= atom()
%%
%% @doc     Returns the child specification for the interface supervisor.
%% @hidden
%% @end
%%--------------------------------------------------------------------
interface_spec_for({iso_interface, Name}) ->
	{Name,
		{iso_interface_sup, start_link, [Name]},
		permanent,
		infinity,
		supervisor,
		[iso_interface_sup] };

interface_spec_for({erl_interface, Name}) ->
	{Name,
		{erl_interface_sup, start_link, [Name]},
		permanent,
		infinity,
		supervisor,
		[erl_interface_sup] }.


