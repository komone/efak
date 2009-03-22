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

%% @doc	Starts the line given a configured name.
%%
%%		The line configration is expected to follow the structure below
%%
%% ```
%%	<line
%%		interface="Name",
%%		module="connection_module">
%%		<acceptor
%%			module="acceptor">
%%			<param_1>value</param_1>
%%			<param_2>value</param_2>
%%			...
%%			<param_N>value</param_N>
%%		</acceptor>
%%		<connector
%%			module="connector">
%%			<param_1>value</param_1>
%%			<param_2>value</param_2>
%%			...
%%			<param_N>value</param_N>
%%		</connector>
%%		<nodes>
%%			<node>onenode@onehost</node>
%%			<node>othernode@otherhost</node>
%%		</nodes>
%%	</line>
%% '''
%%
%%		In a line configuration there should be at least one connector
%% 		or one acceptor, more of both are accepted if useful. Acceptor
%% 		and connector will be started in the current node if the current
%% 		node name is included in the `<nodes>' tag.
%%
%%		Acceptors and connectors are worker processes implemented in the
%% 		module identified by the module attribute (for examples look at
%% 		{@link tcp_acceptor} and {@link tcp_connector}).
%%
%%		Parameters required by acceptor and connector processes will be
%% 		passed via start_link(Params) function, where Params is a list
%% 		of tuples `[{interface,"Name"},{module="connection_module"},
%% 		{param_1,"value"},{param_2,"value"},...,{param_N,"value"}]'.
%%
%%		The line connections are implemented in the module
%% 		identified by the module attribute of the `<line>' tag.
%%
%% 		Look at {@link tcp_connection} for an example.
%% @end

-module(line_sup).

-behaviour(supervisor).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1]).

%%--------------------------------------------------------------------
%% Internal exports - supervisor callbacks
%%--------------------------------------------------------------------
-export([init/1]).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Name) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Pid = pid()
%%
%% @doc		Starts the line supervisor for interface named Name.
%%			The process is registered locally as `NAME_line_sup'
%% 			because there could be more line supervisors in one node.
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
	SupName = list_to_atom(lists:flatten([atom_to_list(Name), "_", atom_to_list(?MODULE)])),
	supervisor:start_link({local, SupName}, ?MODULE, [Name]).

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
init([Name]) ->
	case load_config(Name) of
		[] -> ignore;
		[{line, LineAttr, Content}] ->
			ConnectionModule = list_to_atom(config_server:get_attr_value(module,LineAttr)),
			ConnectionSupName =
			list_to_atom(lists:flatten([atom_to_list(Name), "_conn_sup"])),
			ConnectionSup =
			{ConnectionSupName,
				{simple_worker_sup,	start_link,
					[ConnectionSupName, ConnectionModule]},
				permanent, infinity, supervisor, [simple_worker_sup]},
			WorkersSpec = build_child_spec(LineAttr, Content),
			{ok,
				{{one_for_one, 3, 5},
				[ConnectionSup | WorkersSpec]
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
%% @doc     Returns the simple_xml confugration for the line named Name.
%% @hidden
%% @end
%%--------------------------------------------------------------------
load_config(Name) ->
	Q = lists:flatten(["//line[nodes/node[.='", atom_to_list(node()),"']]"]),
	AllLines = config_server:to_simple_xml(config_server:query_param(Q)),
	NameAsString = atom_to_list(Name),
	lists:filter(fun({line,Attr,_}) ->
					config_server:get_attr_value(interface,Attr) =:= NameAsString end,
					AllLines).

%%--------------------------------------------------------------------
%% @spec	(Contents) -> [Param]
%%
%%				Contents	= list(term())
%%				Param 		= {Key, Value}
%%				Key 		= atom()
%%				Value		= string()
%%
%% @doc     Trasforms parameters from simple_xml format into
%% 			{Key,Value} tuples.
%% @hidden
%% @end
%%--------------------------------------------------------------------
make_param_list(Contents) ->
	lists:map(fun 	({Param,_,[Value]}) -> {Param,Value};
					({Param,_,[]}) 		-> {Param, []} end, Contents).

%%--------------------------------------------------------------------
%% @spec	(LineAttr, Content) -> [ChildSpec]
%%
%%				LineAttr	= [Param]
%%				Contents	= list(term())
%%				Param 		= {Key, Value}
%%				Key 		= atom()
%%				Value		= string()
%%
%% @doc     Trasforms parameters from simple_xml format into
%% 			{Key,Value} tuples.
%% @hidden
%% @end
%%--------------------------------------------------------------------
build_child_spec(LineAttr, Content) ->
	OnlyAcceptorOrConnector =
	lists:filter(fun({X,_,_}) when X =:= acceptor orelse X =:= connector -> true;
					(_) -> false end, Content),
	{_, WorkersSpec} =
	lists:foldl(fun ({Type, Attr, Params}, {Id, List})
					when Type =:= acceptor orelse Type =:= connector ->
				Module = list_to_atom(config_server:get_attr_value(module, Attr)),
				{Id+1,	%% supervisor requires unique child names
					[{Id, {Module, start_link,
						[LineAttr ++ make_param_list(Params)]},
						permanent, brutal_kill, worker, [Module] } | List]}
				end, {0,[]}, OnlyAcceptorOrConnector),
	WorkersSpec.

