-module(ircTime).
-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2]).
-export([handle_info/2, code_change/3]).
-export([date_to_string/1]).
-include("ircParser.hrl").
-import(optimusPrime, [get_Integer/1]).

init(_Args) ->
	{ok, []}.

handle_event(#privmsg{target=Target, from=From, message="#t"}, State) ->
	Message = From ++ ": " ++ date_to_string(erlang:localtime()),
	sendPid ! #privmsg{from=From, target=Target, message=Message},
	{ok, State};

handle_event(#privmsg{target=Target, from=From, message="#t " ++ K}, State) ->
	N = get_Integer(K),
	Message = if
		N < 0 orelse N > 100000000000 -> "Invalid input";
		true -> date_to_string(seconds_to_date(N))
	end,
	sendPid ! #privmsg{from=From, target=Target, message=Message},
	{ok, State};

handle_event(_Msg, State) ->
	{ok, State}.

terminate(_Args, _State) ->
    ok.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% convers the date to a string in the form of <hour>:<minute>:<second>, <day><postfix> of <month>, <year>
date_to_string({Date, Time}) -> 
	{Yeart,Montht,Dayt} = Date,
	{Hourt,Mint,Sect} = Time,
	case Dayt of
		1 -> DayPrfx = "st";
		2 -> DayPrfx = "nd";
		3 -> DayPrfx = "rd";
		_ -> DayPrfx = "th"
	end,
	Month = lists:nth(Montht, ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]), 
	IntToString = fun(A) -> lists:flatten(io_lib:format("~p", [A])) end,
	[Hour, Min, Sec, Day, Year] = lists:map(IntToString, [Hourt, Mint, Sect, Dayt, Yeart]), % aplies IntToString to each element in the list
	Hour ++ ":" ++ Min ++ ":" ++ Sec ++ ", " ++ Day ++ DayPrfx ++ " of " ++ Month ++ ", " ++ Year.

seconds_to_date(K) ->
	BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
	Seconds       = BaseDate + K,
	calendar:gregorian_seconds_to_datetime(Seconds).
