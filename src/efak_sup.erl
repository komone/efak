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

%% @doc	This is the topmost supervisor of the efak application.
%% @end

-module(efak_sup).

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
%% @doc		Starts the main supervisor of the efak application.
%%			Supervised processes are {@link config_server}, {@link interfaces_sup}
%%			and {@link lines_sup}.
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
%% @doc     Starts the interfaces and lines for the application.
%%			If interfaces or lines fail all supervisors are stopped and reloaded.
%%			
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([]) ->
	{ok,
	{
	{one_for_all,0,1},
	[
	{config_server,{config_server,start_link,[]},permanent,5000,worker,[config_server]},
	{interfaces,{interfaces_sup,start_link,[]},permanent,infinity,supervisor,[interfaces_sup]},
	{lines,{lines_sup,start_link,[]},permanent,infinity,supervisor,[lines_sup]}
	]
	}
	}.

%%====================================================================
%% Internal functions
%%====================================================================

