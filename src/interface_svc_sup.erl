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

%% @doc	This supervisor starts processes used by all interfaces in the node.
%% @see line_proxy
%% @see key_timers
%% @end

-module(interface_svc_sup).

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
%%				Pid 	= pid()
%%
%% @doc		Starts the interface service supervisor.
%%			Mnesia is also started from the supervisor and all
%% 			required tables are checked to be ready. The supervisor
%% 			fails to start if needed tables are not ready.
%% @end
%%--------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Behaviour functions
%%====================================================================

%% Timeout has to be synchronized with interfaces_sup timeout
-define(MNESIA_WAIT_TIMEOUT, 60*1000).

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
init(_) ->
	%% return value of mnesia:start is not checked beacuse in case of restart
	%% the error {error, already_started} could be returned ...
	mnesia:start(),

	%% ... but tables need to be ready for operation
	ok = mnesia:wait_for_tables([txtemp, txlog, saf], ?MNESIA_WAIT_TIMEOUT),
	{ok,
		{{one_for_one, 3, 5},
		[
			{timers, {key_timers, start_link, []},
				permanent, brutal_kill, worker, [key_timers]},
			{line_proxy, {line_proxy, start_link, []},
				permanent, brutal_kill, worker, [line_proxy]}
		]
		}
	}.

%%====================================================================
%% Internal functions
%%====================================================================

