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

%% @doc Initialize efak node.
%%
%% 		Use init_node/0 to create the mnesia schema, use init_node/1 to
%% 		create a replica of the schema.
%% @end

-module(efak_ctl).

-export([init_node/0, init_node/1, saf_stats/0, saf_stats/1]).

-define(DBMODULES, [iso_log, iso_saf]).

%%--------------------------------------------------------------------
%% @spec	() -> ok
%%
%% @doc		Creates the mnesia schema and tables on the current node.
%% @end	
%%--------------------------------------------------------------------
init_node() ->
	io:format("Creating mnesia schema on node ~p~n", [node()]),
	case mnesia:create_schema([node()]) of
		ok ->
			io:format("done~n"),
			ok = mnesia:start(),
			lists:foreach(fun(Module) ->
							io:format("Creating table for process ~p~n", [Module]),
							Module:create_table([node()]) end, ?DBMODULES),
			stopped = mnesia:stop(),
			io:format("Node ~p is initialized~n", [node()]),
			ok;
		{error, {Node, {already_exists, Node}}} ->
			io:format("Schema already exists on node ~p~n", [Node]),
			ko;
		{error, Error} ->
			io:format("Error creating schema on node ~p : ~p~n", [node(), Error]),
			ko
	end.

%%--------------------------------------------------------------------
%% @spec	(Node) -> ok
%%
%%				Node = node()
%%
%% @doc		Replicates from the specified node the mnesia schema and
%% 			tables on the current node.
%% @end	
%%--------------------------------------------------------------------
init_node(Node) ->
	ok = mnesia:start(),
    case mnesia:change_config(extra_db_nodes, [Node]) of
	{ok, []} ->
	    %% If Mnesia on the current node already knew to link up with MasterNode,
	    %% change_config says 'ok' but with an empty list. This is however exactly
	    %% the same thing that happens if we fail to connect to the remote node
	    %% because of an distribution mechanism failure so we need to make sure
	    %% we are online...
	    case lists:member(Node, mnesia:system_info(running_db_nodes)) of
		true ->
		    ok;
		false ->
		    erlang:error("Failed connecting to master node - Erlang distribution problem? "
				 "Are you running SSL on the master node but not on this?")
	    end;
	{ok, [Node]} ->
	    ok;
	{error, E} ->
	    erlang:error(E)
    end,
	io:format("Replicating mnesia schema from node ~p~n", [Node]),
	{atomic, ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),
	lists:foreach(fun(Module) ->
					io:format("Replicating table for process ~p~n", [Module]),
					Module:replicate_table() end, ?DBMODULES).
	

%%--------------------------------------------------------------------
%% @spec	() -> Status::list()
%%
%% @doc		Returns the saf status.
%% @end	
%%--------------------------------------------------------------------
saf_stats() ->
	iso_saf:stats().

%%--------------------------------------------------------------------
%% @spec	(Name) -> Status::list()
%%
%%				Name = atom()
%%
%% @doc		Returns the saf status for interface named Name.
%% @end	
%%--------------------------------------------------------------------
saf_stats(Name) when is_atom(Name) ->
	iso_saf:stats(Name).

