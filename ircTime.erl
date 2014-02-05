-module(ircTime).
-export([ircTime/0, date_to_string/1]).
-include("ircParser.hrl").

ircTime() ->
	receive
		die ->
			io:format("timePid :: EXIT~n");
		% [From, _, _, Target, "#t"]  ->
		#privmsg{target=Target, from=From, message="#t"} ->
			Message = From ++ ": " ++ date_to_string(erlang:localtime()),
			sendPid ! #privmsg{from=From, target=Target, message=Message}
	end,
	ircTime().

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
