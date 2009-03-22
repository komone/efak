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

%% @doc	Starts the lines configured for the current node.
%%		If no lines has to be started, it returns `ignore' in the
%% 		init() function.
%%
%%		{@link config_server} is used to retrieve line configuration.
%% @end

-module(lines_sup).

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
%%			ChildSpecs are built according to configuration
%% 			provided by `config_server'.
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
%% @doc     Starts the line supervisors for the current node if any.
%%			Max 3 start retries are allowed within 5 seconds.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([]) ->
	case load_config() of
		[]			-> ignore;
		LineNames	-> {ok, {{one_for_one, 3, 5},
						lists:map(fun line_spec_for/1, LineNames)}}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	() -> [Line]
%%
%%				Line	= atom()
%%
%% @doc     Returns the interface names of the lines to be started
%% 			on the current node.
%% @hidden
%% @end
%%--------------------------------------------------------------------
load_config() ->
	Q = lists:flatten(["//line[nodes/node[.='", atom_to_list(node()),"']]"]),
	Lines = config_server:to_simple_xml(config_server:query_param(Q)),
	lists:map(fun(X) -> list_to_atom(X) end,
				lists:foldl(fun ({line, Attr, _}, Names) ->
								[config_server:get_attr_value(interface, Attr) | Names]
							end, [], Lines)).

%%--------------------------------------------------------------------
%% @spec	(Name) -> LineSpec
%%
%%				Name			= atom()
%% 				LineSpec		= {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% 				Id 				= term()
%%				StartFunc		= {atom(),atom(),[term()]}
%%				Restart 		= permanent | transient | temporary
%%				Shutdown 		= brutal_kill | int() | infinity
%% 				Type 			= worker | supervisor
%% 				Modules 		= [Module] | dynamic
%%  			Module 			= atom()
%%
%% @doc     Returns the child specification for the line supervisor.
%% @hidden
%% @end
%%--------------------------------------------------------------------
line_spec_for(Name) ->
	{Name,
		{line_sup, start_link, [Name]},
		permanent,
		infinity,
		supervisor,
		[line_sup] }.

