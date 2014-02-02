-module(ircParser).
-export([parse/1, lineParse/1]).

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
					case Line of
						% Patern match join command
						[_,_,_,_,"#j " ++ K] ->
							SendPid ! {command, {"JOIN", string:strip(K)}};

						% Patern match quit command
						[_,_,_,_,"#q\r"] ->	
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
							IntToString = fun(A) -> lists:flatten(io_lib:format("~p", [A])) end,
							case Dayt of
								1 -> DayPrfx = "st";
								2 -> DayPrfx = "nd";
								3 -> DayPrfx = "rd";
								_ -> DayPrfx = "th"
							end,
							Month = lists:nth(Montht, ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]),
							[Hour, Min, Sec, Day, Year] = lists:map(IntToString, [Hourt, Mint, Sect, Dayt, Yeart]),
							Message = Hour ++ ":" ++ Min ++ ":" ++ Sec ++ ", " ++ Day ++ DayPrfx ++ " of " ++ Month ++ ", " ++ Year,
							SendPid ! {command, {"PRIVMSG", Target, Message}};		% Send a response back to where it came from.

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
	Message = string:strip(string:sub_word(Str, 2, $:)),
	[From, Host, Command, Target, Message].
