-module(ircParser).
-export([start/1, parse/1, lineParse/1]).
-import(optimusPrime, [starts/1]).

start(SendPid) ->
	register(primePid, spawn(optimusPrime, starts, [SendPid])),
	parse(SendPid).

% starts passing the message around to the different handlers.
parse(SendPid) ->
    receive
		die ->
			io:format("parserPid :: EXIT~n"),
			exit(self(), normal);
		"PING :" ++ T ->
			io:format("Ping: ~s~n", [T]),
			SendPid ! {command, {"PONG", T}};
		T -> 
			Command = string:sub_word(T, 2),
			if 
				% If this is a PRIVMSG parse it as one and go through case on types available
				Command == "PRIVMSG" ->
					Line = lineParse(T),
					primePid ! Line,
					case Line of
						% Patern match join command
						[_,_,_,_,"#j " ++ K] ->
							SendPid ! {command, {"JOIN", string:strip(K)}};

						% Patern match quit command
						[_,_,_,_,"#q"] ->	
							SendPid ! {command, {"QUIT", ":Earl Out"}};

						% Patern match quit command
						[_,_,_,_,"#q" ++ K] ->	
							io:format("~p~n", [K]),
							SendPid ! {command, {"QUIT", ":" ++ K}};

						% Pattern match part command
						[_, _, _, _, "#p " ++ K] ->
							SendPid ! {command, {"PART", string:strip(K)}};

						% Pattern match nick command
						[_, _, _, _, "#n " ++ K] ->
							SendPid ! {command, {"NICK", string:strip(K)}};

						% Pattern match time command
						[_, _, _, Target, "#t" ++ _] ->
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
							Message = Hour ++ ":" ++ Min ++ ":" ++ Sec ++ ", " ++ Day ++ DayPrfx ++ " of " ++ Month ++ ", " ++ Year,
							SendPid ! {command, {"PRIVMSG", Target, Message}};

						% Stop dumb errors if the switch case isn't satisfied
						_Default ->
							false
					end;

				% Else
				true ->
					checkIndentResponce(re:run(T, "NOTICE AUTH :... Got Ident response"), SendPid)
			end
    end,
    parse(SendPid).

% connects tot he server after indent responce
checkIndentResponce({match, [_]}, SendPid) ->
	SendPid ! {command, {"USER", "Sir_Earl Sir_Earl Sir_Earl Sir_Earl"}},
	SendPid ! {command, {"NICK", "Earl2"}},
	true;
checkIndentResponce(_,_) ->
	false.

lineParse(Str) ->
	From = string:sub_word(string:sub_word(Str, 1, $:), 1, $!),
	Host = string:sub_word(string:sub_word(Str, 2, $!), 1),
	Command = string:sub_word(Str, 2),
	Target = string:sub_word(Str, 3),
	Message = string:strip(string:strip(string:sub_word(Str, 2, $:)), both, $\r),
	[From, Host, Command, Target, Message].
