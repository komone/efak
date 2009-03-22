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

%% @doc	A generic simple_one_for_one supervisor.
%%		It is used to start temporary processes of the same module.
%% @end
-module(simple_worker_sup).

-behaviour(supervisor).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1, start_link/2, start_link/3]).

%%--------------------------------------------------------------------
%% Internal exports - supervisor callbacks
%%--------------------------------------------------------------------
-export([init/1]).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Module) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Module	= atom()
%%				Pid		= pid()
%%
%% @doc		Starts the supervisor without registering.
%% @end	
%%--------------------------------------------------------------------
start_link(Module) when is_atom(Module) ->
	supervisor:start_link(?MODULE, Module).

%%--------------------------------------------------------------------
%% @spec	(Name, Module) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Name	= atom()
%%				Module	= atom()
%%				Pid 	= pid()
%%
%% @doc		Starts the supervisor with child of type Module and locally registers it as Name.
%% @end	
%%--------------------------------------------------------------------
start_link(Name, Module) when is_atom(Name), is_atom(Module) ->
	supervisor:start_link({local, Name}, ?MODULE, Module).

%%--------------------------------------------------------------------
%% @spec	(Name, Module, ChildArgs) -> {ok, Pid} | ignore | {error, Error}
%%
%%				Name		= atom()
%%				Module		= atom()
%%				ChildArgs	= list()
%%				Pid 		= pid()
%%
%% @doc		Starts the supervisor with child of type Module and locally registers it as Name.
%%			ChildArgs is a list of parameters that is passed to the child processes when started.
%% @end	
%%--------------------------------------------------------------------
start_link(Name, Module, Args) when is_atom(Name), is_atom(Module), is_list(Args) ->
	supervisor:start_link({local, Name}, ?MODULE, [Module, Args]).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Args) ->
%%				{ok,  {SupFlags,  [ChildSpec]}}	|
%%				ignore							|
%%				{error, Reason}
%%
%%				Args		= Module | list()
%%				Module		= atom()
%%				ChildArgs	= list()
%%
%% @doc     Initialize a simple_one_for_one supervisor with child of type Module.
%%			ChildArgs are passed to child's start_link.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(Module) when is_atom(Module) ->
	{ok, {{simple_one_for_one, 0, 1},
          [{Module, {Module, start_link, []},
            temporary, brutal_kill, worker, [Module]}]}};

init([Module, ChildArgs]) when is_atom(Module), is_list(ChildArgs) ->
	{ok, {{simple_one_for_one, 0, 1},
          [{Module, {Module, start_link, ChildArgs},
            temporary, brutal_kill, worker, [Module]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================

