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

%% @doc	A server for managing distributed application configuration
%%		in an erlang cluster.
%%
%% == Introduction ==
%%
%% The config_server is made to be included in erlang applications that operate on different nodes. The config_server represent a solution for having a human-readable and easy changeable configuration file in your application.
%%
%% The config_server provides an API to load, save and query the configuration file whithin your distributed application and, if configuration data is expected to change often, the config_server can also inform registered processes on configuration updates.
%%
%% Application configuration is expected to be included in an XML file that must be stored at least on one node. The configuration file can be inspected by interested processes using XPath queries ([http://www.w3.org/TR/xpath]).
%%
%% == Usage ==
%%
%% === How to include the config_server in your application ===
%%
%% The config_server is an OTP gen_server that can be included in your main application supervisor. The config_server should be included in every node of your application cluster.
%%During the start-up phase the config_server try to load the XML configuration file specified in the application parameter <code>config_server</code>. Possible values of the <code>config_server</code> parameter are the tuples <code>{file, FileName}</code> and <code>{file, none}</code>.
%% If a configuration file is specified and it can not be loaded and/or parsed, the config_server will stop with an error. If no configuration file is provided, the config_server will try to load the configuration file via other config_server  running on other nodes of your application cluster.
%%
%% An example .app file follows
%%
%%<pre>
%%{application, your_app,
%% [{description, "Best App Ever"},
%%  {vsn, "1"},
%%  {modules, [your_app, your_sup, your_module]},
%%  {registered, [your_module]},
%%  {applications, [kernel, stdlib, sasl]},
%%  {mod, {your_app,[]}},
%%  {env, [{config_server, {file, "~/your_conf.xml"}}]}
%% ]}.
%%</pre>
%%
%% === How to interac with the configuration ===
%%
%% The XML configuration file can have an arbitrary structure but must be a valid XML file. According to the structure of your XML file you can ask the config_server for all or part of your file using XPath query strings via {@link config_server:query_param/1}.
%%
%% <code>config_server:query_param/1</code> returns a list of matched xmerl_xml elements that can be easily converted into <code>simple_form</code> (tuple representation of xml) for easier parsing with {@link config_server:to_simple_xml/1}.
%%
%% New version of the configuration file can be loaded with {@link config_server:load_file/1} and saved with {@link config_server:save/0}. While saving the current configuration on one node, all nodes will be asked to save the configuration on disk.
%%
%% {@link config_server:register_for_updates/0} may be used by processes to receive notification on updates.
%%@end

-module(config_server).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0, start/0]).
-export([to_simple_xml/1, query_param/1, load_file/1, save/0, stop/0,
		register_for_updates/0, get_attr_value/2, parse_params/1]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("xmerl/include/xmerl.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {
			conf,				%% xmerl_xml element containing configuration
			registered_pids=[]	%% list of pids looking for configuration updates
		}).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	() -> {ok, Pid} | ignore | {error, Error}
%%
%%				Pid = pid()
%%  			Error = {already_started,Pid} | term()
%%
%% @doc		Starts the server and registers it locally on the current node.
%% @end	
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec	() -> {ok, Pid} | ignore | {error, Error}
%%
%%				Pid = pid()
%%  			Error = {already_started,Pid} | term()
%%
%% @doc		Starts the server as a stand-alone process and registers it locally on the current node.
%%
%%			Stand-alone interface is provided for debugging and maintenance purposes, allowing interaction with the current configuration from the erlang interpreter.
%% @end	
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec	() -> ok
%%
%% @doc		Stops the server when started as a stand-alone process.
%% @end
%%--------------------------------------------------------------------
stop() ->
	gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% @spec	() -> ok
%%
%% @doc		Registers the calling process for receiving notification in case of updates to the configuration.
%%			The calling process will be notified with a {config_server, update} message.
%%			OTP style processes (gen_server,gen_fsm,gen_event) can trap notifications via
%%
%%			<code>handle_info({config_server, update}, State)</code>
%%
%%			callback function and then query the config_server for parameters.
%%			Registered processes gets linked to the config_server.
%% @end
%%--------------------------------------------------------------------
register_for_updates() ->
	gen_server:cast(?MODULE, {register_for_updates, self()}).

%%--------------------------------------------------------------------
%% @spec	(Doc) -> term() | list()
%%
%%				Doc = term() | list(term())
%%
%% @doc		Converts xmerl parsed XML into XML Simple Form as defined in the xmerl module.
%%			Doc may be a list() of parsed XML elements, in such case a list of Simple Form terms is returned.
%% @end	
%%--------------------------------------------------------------------
to_simple_xml(Doc) when is_list(Doc) ->
	lists:map(fun(D) -> to_simple_xml(D) end, Doc);

to_simple_xml(Doc) ->
	strip_whitespace(xmerl_lib:simplify_element(Doc)).

%%--------------------------------------------------------------------
%% @spec	(String) -> list(term()) | {error, noconf}
%%
%%				String = string()			
%%
%% @doc		Executes the XPath query defined in String on the XML configuration file and returns the matched elements.
%% @end	
%%--------------------------------------------------------------------
query_param(String) ->
	gen_server:call(?MODULE, {xpath_query, String}).

%%--------------------------------------------------------------------
%% @spec	(FileName) -> {ok, Replies}
%%
%%				FileName	= string()
%%				Replies 	= [{Node,Reply}]
%%				Node		= atom()
%%				Reply		= ok | ko
%%
%% @doc		Loads the new configuration file FileName on the current and connected nodes. A list of successfully updated nodes is returned in Replies.
%%			Processes registered for update will be notified.
%% @end	
%%--------------------------------------------------------------------
load_file(FileName) ->
	gen_server:call(?MODULE, {load_file, FileName}).

%%--------------------------------------------------------------------
%% @spec	() -> {Replies, BadNodes}
%%
%%				Replies 	= [{Node,Reply}]
%%				Node		= atom()
%%				Reply		= ok | {error, Reason}
%%				BadNodes	= [Node]
%%				Reason		= posix()
%%
%% @doc		Saves the current configuration in the XML files on all connected nodes.
%% @end	
%%--------------------------------------------------------------------
save() ->
	gen_server:multi_call(?MODULE, save).

%%--------------------------------------------------------------------
%% @spec	(Name, AttrList) -> Value
%%
%%				Name		= atom()
%%				AttrList	= [Attr]
%%				Attr 		= {Name, Value}
%%
%% @doc     Search for an attribute value from an attribute list.
%% @end
%%--------------------------------------------------------------------
get_attr_value(Name, AttrList) ->
	{value, {_, Value}} = lists:keysearch(Name, 1, AttrList),
	Value.

%%--------------------------------------------------------------------
%% @spec	(Param) -> {Key, Value2}
%%
%%				Param 	= {Key, Value}
%%				Key		= atom()
%%				Value	= string()
%%				Value2 	= int() | atom() | ip_address() "parsed parameters"
%%
%% @doc     Convert string values according to parameter Key.
%% @end
%%--------------------------------------------------------------------
parse_params({Any,[]}) ->
	Any;

parse_params({ip, String}) ->
	{ok, Value} = inet_parse:address(String),
	{ip, Value};

parse_params({Key, Value}) ->
	case all_digits(Value) of
		true	-> {Key, list_to_integer(Value)};
		false	-> {Key, list_to_atom(Value)}
	end.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Args) ->
%%				{ok, State}				|
%%				{ok, State, Timeout}	|
%%				ignore					|
%%				{stop, Reason}
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec	(Args) -> {ok, State} | {stop, noconf}
%%
%% @doc     Reads and parse the configuration from XML file or try to load the configuration from the connected nodes.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	case load() of
	    {ok, Conf} ->	{ok, #state{ conf=Conf }};
		_ ->			{stop, noconf}	
	end.

%%--------------------------------------------------------------------
%% @spec	handle_call(Request, From, State) ->
%%				{reply, Reply, State}			|
%%				{reply, Reply, State, Timeout}	|
%%				{noreply, State}				|
%%				{noreply, State, Timeout}		|
%%				{stop, Reason, Reply, State}	|
%%				{stop, Reason, State}
%%
%% @doc		Handling call messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec	({load_file, FileName}, From, State) ->
%%				{reply, {Reply, Replies}, State}
%%
%%				FileName	= string()
%%				Replies 	= [{Node,Reply}]
%%				Node		= atom()
%%				Reply		= ok | ko
%%			
%% @doc		Loads the new configuration file FileName on the current and connected nodes. A list of successfully updated nodes is returned in Replies.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({load_file, FileName}, _From, #state{registered_pids=Pids} = State) ->
	{ok, Conf} = load(FileName),
	{Replies,_}= gen_server:multi_call(nodes(), ?MODULE, {update, Conf}),
	Reply =
	case lists:all(fun ({_,ok})		-> true;
						(_) 		-> false
					end, Replies) of
		true -> ok;
		false -> ko
	end,
	notify_update(Pids),
	{reply, {Reply,Replies}, State#state{conf=Conf}};

%%--------------------------------------------------------------------
%% @spec	({update, Conf}, From, State) ->
%%				{reply, ok, State}
%%
%%				Conf	= term{}
%%			
%% @doc		Updates the current state with the configuration Conf and notify registered processes.
%% 			Conf is a xmerl parsed XML.
%%			
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({update, Conf}, _From, #state{registered_pids=Pids} = State) ->
	notify_update(Pids),
	{reply, ok, State#state{conf=Conf}};

%%--------------------------------------------------------------------
%% @spec	(save, From, State) ->
%%				{reply, Reply, State}
%%
%%				Reply	= ok | {error, Reason}
%%				Reason	= posix()
%%			
%% @doc		Writes the current configuration on disk if the config_server has a XML config file.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(save, _From, #state{conf=Conf} = State) ->
	Reply = save(Conf),
	{reply, Reply, State};

%%--------------------------------------------------------------------
%% @spec	({xpath_query, String}, From, State) ->
%%				{reply, Reply, State}
%%
%%				Reply	= list(term()) | {error, noconf}
%%				String	= string()
%%			
%% @doc		Executes the XPath query String on the current configuration and returns the result.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({xpath_query, _String}, _From, #state{conf=[]} = State) ->
	Reply = {error, noconf},
	{reply, Reply, State};

handle_call({xpath_query, String}, _From, #state{conf=Conf} = State) ->
	Reply = xmerl_xpath:string(String, Conf),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec	handle_cast(Msg, State) ->
%%				{noreply, State}			|
%%				{noreply, State, Timeout}	|
%%				{stop, Reason, State}
%%
%% @doc		Handling cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec	(stop, State) -> {stop, normal, State}
%%
%% @doc		Stops the server when started as a stand-alone process.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State};

%%--------------------------------------------------------------------
%% @spec	({register_for_updates, Pid}, State) -> {noreply, State}
%%
%%				Pid = pid()
%%
%% @doc		Link the Pid to trap exits and insert it in the registered pid list.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({register_for_updates, Pid}, #state{registered_pids=Pids} = State) ->
	NewPidList =
	case lists:member(Pid, Pids) of
		false ->
			link(Pid),
			[Pid | Pids];
		true ->
			Pids
	end,
	{noreply, State#state{registered_pids=NewPidList}};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @spec	handle_info(Msg, State) ->
%%				{noreply, State}			|
%%				{noreply, State, Timeout}	|
%%				{stop, Reason, State}
%%
%% @doc		Handling all non call/cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec	({'EXIT', Pid, Reason}, State) -> {noreply, State}
%%
%% @doc		Check for 'EXIT' signals of registered processes and clean up the notify list.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, #state{registered_pids=Pids} = State) ->
	NewPidList =
	case lists:member(Pid, Pids) of
		true ->
			unlink(Pid),
			lists:delete(Pid, Pids);
		false ->
			Pids
	end,
	{noreply, State#state{registered_pids=NewPidList}};

handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @spec	(Reason, State) -> term() "ignored by gen_server"
%%
%% @doc		This function is called by a gen_server when it is about
%%			to terminate. It should be the opposite of Module:init/1
%%			and do any necessary cleaning up. When it returns, the
%%			gen_server terminates with Reason.
%%			The return value is ignored.
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{registered_pids=Pids}) ->
	%% remove the established links
	lists:map(fun(P) -> unlink(P) end, Pids),
	ok.

%%--------------------------------------------------------------------
%% @spec	(OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc		Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec	(Element) -> StrippedElement
%%
%%				StrippedElement	= Xmerl_xml
%%				Element 		= Xmerl_xml
%%				Xmerl_xml 		= {atom(), list(), list()}
%%
%% @doc		Deletes empty (only whitespace) content from the xmerl Element.
%%
%%			Original code is from [http://arandomurl.com/post/Simple-XML-in-Erlang]
%% @hidden
%% @end
%%--------------------------------------------------------------------
strip_whitespace({El,Attr,Children}) ->
	NChild = lists:filter(fun(X) ->
		case X of
			" " -> false;
			_   -> true
		end  end, Children),
	Ch = lists:map(fun(X) -> strip_whitespace(X) end, NChild),
	{El,Attr,Ch};

%% Just a Plain Value
strip_whitespace(String) -> String.

%%--------------------------------------------------------------------
%% @spec	() -> {ok, Conf} | ko
%%
%%				Conf		= term() | EmptyList
%%				EmptyList	= []
%%
%% @doc		Loads the xmerl parsed configuration Conf from the file specified in the environment variable 'config_server' or try to load the configuration from remote config_servers.
%%
%%			If the 'config_server' environment variable is not found and no remote configuration could be retrieved, an empty list is returned. This allows to start a config_server as a stand-alone process to interact with others config_server in the cluster.
%% @hidden
%% @end
%%--------------------------------------------------------------------
load() ->
	case application:get_env(config_server) of
		{ok, {file,none}} -> remote_load();
		{ok, {file,FileName}} -> load(FileName);
		undefined ->
			case remote_load() of
			    ko			-> {ok, []};
				RemoteConf	-> RemoteConf
			end
	end.

%%--------------------------------------------------------------------
%% @spec	(FileName) -> {ok, Conf}
%%
%%				Conf		= term()
%%
%% @doc		Loads the xmerl parsed configuration Conf from the file FileName.
%% @hidden
%% @end
%%--------------------------------------------------------------------
load(FileName) ->
	{Conf,_} = xmerl_scan:file(FileName,[{space,normalize},{encoding,"utf-8"}]),
	{ok, Conf}.

%%--------------------------------------------------------------------
%% @spec	(Conf) -> ok | {error, Reason}
%%
%%				Conf	= term()
%%				Reason	= posix()
%%
%% @doc		Saves the configuration Conf in the XML file.
%% @hidden
%% @end
%%--------------------------------------------------------------------
save(Conf) ->
	case application:get_env(config_server) of
		{ok, {file,none}} ->
			ok;
		{ok, {file,FileName}} ->
			file:write_file(FileName, list_to_binary(xmerl:export([Conf], xmerl_xml)));
		undefined -> 
			ok
	end.

%%--------------------------------------------------------------------
%% @spec	() -> {ok, Conf} | ko
%%
%%				Conf	= term()
%%
%% @doc		Retrieves the configuration from remote connected nodes.
%% @hidden
%% @end
%%--------------------------------------------------------------------
remote_load() ->
	{Replies,_} = gen_server:multi_call(nodes(), ?MODULE, {xpath_query, "/*"}),
	first_valid_reply(Replies).

%%--------------------------------------------------------------------
%% @spec	(Replies) -> {ok, Conf} | ko
%%
%%				Conf		= term()
%%				Replies		= {Node,Reply}
%%				Node 		= atom()
%%				Reply		= list(term())
%%
%% @doc		Finds the first valid configuration returned by the remote connected nodes. It is assumed that the XML configuration is composed by only one root element.
%% @hidden
%% @end
%%--------------------------------------------------------------------
first_valid_reply([]) ->
	ko;

first_valid_reply([{_,L} | _]) when is_list(L), length(L) =:= 1 ->
	[Conf] = L,
	{ok, Conf};

first_valid_reply([_ | Rest]) ->
	first_valid_reply(Rest).

%%--------------------------------------------------------------------
%% @spec	(Pids) -> ok
%%
%%				Pids	= list(pid())
%%
%% @doc		Send a message to notify an an update to the configuration.
%% @hidden
%% @end
%%--------------------------------------------------------------------
notify_update(Pids) ->
	lists:map(fun(P) -> P ! {?MODULE, update} end, Pids),
	ok.

%%--------------------------------------------------------------------
%% @spec	(List) -> true | false
%%
%%				List	= string()
%%
%% @doc     Checks if the string contains only numeric digits.
%% @hidden
%% @end
%%--------------------------------------------------------------------
all_digits(List) when is_list(List) ->
	lists:all(fun
				(C) when C >= $0 andalso C =< $9 -> true;
				(_) -> false
				end, List).

