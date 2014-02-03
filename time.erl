-module(time).
-export([time/1]).

time(SendPid) ->
	receive
		die ->
			io:format("timePid :: EXIT");
		[From, _, _, Target, "#t" ++ _]  ->
			{{Yeart,Montht,Dayt},{Hourt,Mint,Sect}} = erlang:localtime(),
							case Dayt of
								1 -> DayPrfx = "st";
								2 -> DayPrfx = "nd";
								3 -> DayPrfx = "rd";
								_ -> DayPrfx = "th"
							end,

							Month = lists:nth(Montht, ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]), 
							IntToString = fun(A) -> lists:flatten(io_lib:format("~p", [A])) end, % converts the numbers from 5 -> "5"
							[Hour, Min, Sec, Day, Year] = lists:map(IntToString, [Hourt, Mint, Sect, Dayt, Yeart]), % aplies IntToString to each element in the list
							Message = From ++ ": " ++ Hour ++ ":" ++ Min ++ ":" ++ Sec ++ ", " ++ Day ++ DayPrfx ++ " of " ++ Month ++ ", " ++ Year,
							case Target of
								"#" ++ _ ->
									SendPid ! {command, {"PRIVMSG", Target, Message}};
								_ ->
									SendPid ! {command, {"PRIVMSG", From, Message}}
							end
	end,
	time(SendPid).