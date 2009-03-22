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

%%
%% @doc Application module of the bg_example application.
%%
%% 	The application demonstrate how to build applications with efak.
%% @see bg_example
%% @end

-module(bg_example_app).

-behaviour(application).

%%--------------------------------------------------------------------
%% Internal exports - application callbacks
%%--------------------------------------------------------------------
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Type, StartArgs) ->
%%				{ok, Pid}			|
%%				{ok, Pid, State}	|
%%				{error, Reason}
%%
%% @doc     This function is called whenever an application is started
%%			using application:start/1,2, and should start the processes
%%			of the application. If the application is structured
%%			according to the OTP design principles as a supervision
%%			tree, this means starting the top supervisor of the tree.
%% @hidden
%% @end
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    case bg_example_sup:start_link() of
		{ok, Pid} -> 
			{ok, Pid};
		Error ->
			Error
    end.

%%--------------------------------------------------------------------
%% @spec	(State) -> void()
%%
%% @doc     This function is called whenever an application has
%%			stopped. It is intended to be the opposite of
%%			Module:start/2 and should do any necessary cleaning up.
%%			The return value is ignored.
%% @hidden
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

