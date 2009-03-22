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

%% @doc Iso Log Browser
%%
%% 		A very basic tool to query the interface log.
%% @end

-module(ilb).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/0, start/1, stop/0, select/1, select/2, select/3, show/0,
		show/1, list/0, list/1, stats/0, stats/1]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("txlog.hrl").

-define(LISTSIZE, 20).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {
			max = 1000,
			data,
			pos
		}).

%%====================================================================
%% External functions
%%====================================================================

%% @spec () -> ok
%% @doc Starts the log browser process
%% @end
start() ->
	start([]).

%% @spec (Args) -> ok
%% 		Args = list()
%% @doc Starts the log browser process
%% @end
start(Args) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Args], []).

%% @spec () -> ok
%% @doc Stops the log browser process
%% @end
stop() ->
	gen_server:cast(?MODULE, stop).

%% @spec (Period) -> ok
%% 		Period = today | yesterday | last_hour | last_min | datetime()
%% @doc Selects all records starting from the specified Period.
%% @end
select(Period) when is_atom(Period) ->
	select(abbrev(Period));
select(Period) when is_tuple(Period) ->
	select(Period, calendar:local_time()).

%% @spec (Period1, [Period2 | Filters]) -> ok
%% 		Period1 = today | yesterday | last_hour | last_min | datetime()
%% 		Period2 = today | yesterday | last_hour | last_min | datetime()
%%		Filters = list({FieldId,FilterValue})
%%		FieldId = int()
%%		FilterValue = int() | string()
%% @doc Selects all records starting from the specified Period.
%% 		Filters may be used to query for specific records.
%% @end
select(Period1, Period2) when is_tuple(Period1), is_tuple(Period2) ->
	select(Period1, Period2, []);
select(Period, Filters) when is_tuple(Period), is_list(Filters) ->
	select(Period, calendar:local_time(), Filters);
select(Period, Filters) when is_atom(Period), is_list(Filters) ->
	select(abbrev(Period), Filters).

%% @spec (Period1, Period2, Filters) -> ok
%% 		Period1 = today | yesterday | last_hour | last_min | datetime()
%% 		Period2 = today | yesterday | last_hour | last_min | datetime()
%%		Filters = list({FieldId,FilterValue})
%%		FieldId = int()
%%		FilterValue = int() | string()
%% @doc Selects all records starting from Period1 and ending in Period2.
%% 		Filters are used to query for specific records.
%% @end
select(Period1, Period2, Filters)
	when is_tuple(Period1), is_tuple(Period2), is_list(Filters) ->
	gen_server:call(?MODULE, {select, Period1, Period2, Filters}, infinity).

%% @spec () -> ok
%% @doc Display the next selected record.
%% @end
show() -> gen_server:call(?MODULE, show).

%% @spec (N) -> ok
%%		N = int()
%% @doc Display the record number N.
%% @end
show(N) -> gen_server:call(?MODULE, {show, N}).

%% @spec () -> ok
%% @doc Lists the next selected record.
%% @end
list() -> gen_server:call(?MODULE, list).

%% @spec (N) -> ok
%% 		N = int()
%% @doc Lists the next N selected records.
%% @end
list(N) -> gen_server:call(?MODULE, {list, N}).

%% @hidden
stats() -> stats(hour).

%% @hidden
stats(Period) -> gen_server:call(?MODULE, {stats,Period}).

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
init(Args) ->
	Max = proplists:get_value(max, Args, 1024*1024*10),
	{ok, #state{ max = Max }}.

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
handle_call({select, DateTime1, DateTime2, Filters}, _From,
			#state{max=Max,data=undefined} = State) ->
	Data = do(query_handle(DateTime1, DateTime2, Filters), Max),
	io:format("~w record(s) found~n", [length(Data)]),
	{reply, ok, State#state{data=Data, pos=1}};

handle_call({select, DateTime1, DateTime2, Filters}, _From,
			#state{max=Max} = State) ->
	NewData = do(query_handle(DateTime1, DateTime2, Filters), Max),
	io:format("~w record(s) found~n", [length(NewData)]),
	{reply, ok, State#state{data=NewData, pos=1}};

handle_call(show, _From, #state{data=Data,pos=I} = State) when I =< length(Data) ->
	print_record(lists:nth(I, Data)),
	{reply, ok, State#state{pos=I+1}};

handle_call({show, N}, _From, #state{data=Data} = State) when N =< length(Data) ->
	print_record(lists:nth(N, Data)),
	{reply, ok, State#state{pos=N+1}};

handle_call(list, _From, #state{data=Data,pos=I} = State) when I+?LISTSIZE =< length(Data) ->
	io:format("~n\t\ttime\t\tinterface\ttype\tMTI~n~s~n", [string:copies("=",75)]),
	[ print_list(X) || X <- lists:sublist(Data, I, ?LISTSIZE) ],
	{reply, ok, State#state{pos=I+?LISTSIZE}};

handle_call(list, _From, #state{data=Data,pos=I} = State) when I =< length(Data) ->
	io:format("~n\t\ttime\t\tinterface\ttype\tMTI~n~s~n", [string:copies("=",75)]),
	[ print_list(X) || X <- lists:sublist(Data, I, length(Data)-I+1) ],
	{reply, ok, State#state{pos=length(Data)+1}};

handle_call({list, N}, _From, #state{data=Data,pos=I} = State) when I+N =< length(Data) ->
	io:format("~n\t\ttime\t\tinterface\ttype\tMTI~n~s~n", [string:copies("=",75)]),
	[ print_list(X) || X <- lists:sublist(Data, I, N) ],
	{reply, ok, State#state{pos=I+N}};

handle_call({stats, Period}, _From, #state{data=Data} = State) ->
	%% io:format("~n\t\ttime\t\tinterface\ttype\tMTI~n~s~n", [string:copies("=",75)]),
	Stats = collect_stats(Period, Data),
	{reply, Stats, State};

handle_call(_Request, _From, State) ->
	io:format("no record found~n"),
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
handle_cast(stop, State) ->
	{stop, normal, State};

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
terminate(_Reason, _State) ->
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

query_handle(DateTime1, DateTime2, Filters) ->
	qlc:sort(qlc:q([Rec || #txlog{ts=T} = Rec <- mnesia:table(txlog),
					calendar:now_to_local_time(T) >= DateTime1,
					calendar:now_to_local_time(T) =< DateTime2,
					match(Rec, Filters)]),
			[{order, fun by_time/2}]).

do(Q, Max) ->
    F = fun() -> qlc:e(Q, [{max_list_size, Max}]) end,
    {atomic, Val} = mnesia:transaction(F),
    {_,L} = lists:foldl(fun(X,{I,R}) -> {I+1,[X#txlog{tx_key=I}|R]} end, {1,[]}, Val),
    lists:reverse(L).

match(#txlog{tx=M}, Filters) ->
	match(M, Filters, []).

match(_, [], Results) ->
	lists:all(fun(X) -> X end, Results);
match(M, [{all, Fields} | Rest], Results) when is_list(Fields) ->
	match(M, Rest, [lists:all(fun(X) -> field_eq(M,X) end, Fields) | Results]);
match(M, [{any, Fields} | Rest], Results) when is_list(Fields) ->
	match(M, Rest, [lists:any(fun(X) -> field_eq(M,X) end, Fields) | Results]);
match(M, [Field | Rest], Results) when is_tuple(Field) ->
	match(M, Rest, [field_eq(M,Field) | Results]).

field_eq(M, {Field,Value}) ->
	case iso:has_field(Field, M) of
		true ->	compare(iso:get_field(Field,M),Value);
		false -> false
	end.

compare(Value1, Value2) when is_integer(Value1), is_integer(Value2) ->
	Value1 =:= Value2;
compare(Value1, Value2) when is_list(Value1), is_integer(Value2) ->
	list_to_integer(Value1) =:= Value2;
compare(Value1, Value2) when is_binary(Value1), is_integer(Value2) ->
	compare(binary_to_list(Value1), Value2);
compare(Value1, Value2) when is_list(Value1), is_list(Value2) ->
	Value1 =:= Value2;
compare(Value1, Value2) when is_binary(Value1), is_list(Value2) ->
	compare(binary_to_list(Value1), Value2).

by_time(#txlog{ts=T1}, #txlog{ts=T2}) ->
	T1 < T2.

print_record(#txlog{tx_key=N, ts=T, elapsed=E, interface=I, type=Type, tx=M }) ->
	{{Y,Mo,D},{H,Mi,S}} = calendar:now_to_local_time(T),
	Msg = lists:keysort(1, M),
	io:format("~n#~w\t~w\t~w\t~4..0w-~2..0w-~2..0w-~2..0w:~2..0w:~2..0w\tElapsed msec: ~w~n~s~n",
				[N,I,Type,Y,Mo,D,H,Mi,S,E,string:copies("=",75)]),
	[io:format("~4w : ~s~n", [F,iso_util:to_list(V)]) || {F,V} <- Msg].

print_list(#txlog{tx_key=N, ts=T, interface=I, type=Type, tx=M }) ->
	{{Y,Mo,D},{H,Mi,S}} = calendar:now_to_local_time(T),
	io:format("#~w\t~4..0w-~2..0w-~2..0w-~2..0w:~2..0w:~2..0w\t~w\t~w\t~s~n",
				[N,Y,Mo,D,H,Mi,S,I,Type,iso_util:to_list(iso:get_MTI(M))]).

abbrev(today) ->
	{Day,_} = calendar:local_time(),
	{Day,{0,0,0}};
abbrev(yesterday) ->
	{Day,_} = calendar:local_time(),
	Yesterday = calendar:date_to_gregorian_days(Day) - 1,
	{calendar:gregorian_days_to_date(Yesterday),{0,0,0}};
abbrev(last_hour) ->
	Now = calendar:local_time(),
	LastH = calendar:datetime_to_gregorian_seconds(Now) - 3600,
	calendar:gregorian_seconds_to_datetime(LastH);
abbrev(last_min) ->
	Now = calendar:local_time(),
	LastM = calendar:datetime_to_gregorian_seconds(Now) - 60,
	calendar:gregorian_seconds_to_datetime(LastM).

collect_stats(sec, Data) ->
	{_, Stats} = lists:foldl(fun stats_per_sec/2, {{{0,0,0},{0,0,0}}, dict:new()}, Data),
	lists:keysort(1, dict:to_list(Stats));
collect_stats(min, Data) ->
	Secs = collect_stats(sec, Data),
	{_, Stats} = lists:foldl(fun stats_per_min/2, {{{0,0,0},{0,0,0}}, dict:new()}, Secs),
	lists:keysort(1, dict:to_list(Stats));
collect_stats(hour, Data) ->
	Secs = collect_stats(sec, Data),
	{_, Stats} = lists:foldl(fun stats_per_hour/2, {{{0,0,0},{0,0,0}}, dict:new()}, Secs),
	lists:keysort(1, dict:to_list(Stats)).

stats_per_sec(#txlog{ts=T,type=data,elapsed=E}, {Current, Stats}) ->
	case calendar:now_to_local_time(T) of
		Current ->
			{Num, Max, Min, Avg, Error} = dict:fetch(Current, Stats),
			S = {Num+1, max(Max,E), min(Min,E), wavg(Num,Avg,E), Error},
			{Current, dict:store(Current, S, Stats)};
		New ->
			{New, dict:store(New, {1, E, E, E, 0}, Stats)}
	end;
stats_per_sec(#txlog{ts=T,type=_,elapsed=E}, {Current, Stats}) ->
	case calendar:now_to_local_time(T) of
		Current ->
			{Num, Max, Min, Avg, Error} = dict:fetch(Current, Stats),
			S = {Num+1, max(Max,E), min(Min,E), wavg(Num,Avg,E), Error+1},
			{Current, dict:store(Current, S, Stats)};
		New ->
			{New, dict:store(New, {1, E, E, E, 1}, Stats)}
	end.

max(X,Y) when X > Y -> X;
max(_,Y) -> Y.

min(X,Y) when X < Y -> X;
min(_,Y) -> Y.

wavg(Num,X,Y) -> ((Num * X) + Y) / (Num+1).
wavg(NumX,X,NumY,Y) -> ((NumX * X) + (NumY * Y)) / (NumX+NumY).

stats_per_min({Sec, {SNum, SMax, SMin, SAvg, SError}}, {Current, Stats}) ->
	{CurDate,{H,Mi,_}} = Current,
	case Sec of
		{CurDate,{H,Mi,_}} -> 
			{Num, Max, Min, Avg, Error} = dict:fetch({CurDate,{H,Mi,0}}, Stats),
			S = {SNum+Num, max(Max,SMax), min(Min,SMin), wavg(Num,Avg,SNum,SAvg), Error+SError},
			{Current, dict:store({CurDate,{H,Mi,0}}, S, Stats)};
		{Date,{HH,MM,_}} ->
			{{Date,{HH,MM,0}}, dict:store({Date,{HH,MM,0}}, {SNum, SMax, SMin, SAvg, SError}, Stats)}
	end.

stats_per_hour({Sec, {SNum, SMax, SMin, SAvg, SError}}, {Current, Stats}) ->
	{CurDate,{H,_,_}} = Current,
	case Sec of
		{CurDate,{H,_,_}} -> 
			{Num, Max, Min, Avg, Error} = dict:fetch({CurDate,{H,0,0}}, Stats),
			S = {SNum+Num, max(Max,SMax), min(Min,SMin), wavg(Num,Avg,SNum,SAvg), Error+SError},
			{Current, dict:store({CurDate,{H,0,0}}, S, Stats)};
		{Date,{HH,_,_}} ->
			{{Date,{HH,0,0}}, dict:store({Date,{HH,0,0}}, {SNum, SMax, SMin, SAvg, SError}, Stats)}
	end.

