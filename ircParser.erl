-module(ircParser).
-export([start/1, parse/1, lineParse/1]).
-import(optimusPrime, [optimusPrime/1]).
-import(time, [time/1]).
-include_lib("eunit/include/eunit.hrl").

start(SendPid) ->
	register(primePid, spawn(optimusPrime, optimusPrime, [SendPid])),
	register(timePid, spawn(time, time, [SendPid])),
	parse(SendPid).

% starts passing the message around to the different handlers.
parse(SendPid) ->
    receive
		die ->
			io:format("parserPid :: EXIT~n"),
			primePid ! die,
			exit(self(), normal);
		"PING :" ++ T ->
			SendPid ! {command, {"PONG", T}};
		T -> 
			Command = string:sub_word(T, 2),
			if 
				% If this is a PRIVMSG parse it as one and go through case on types available
				Command == "PRIVMSG" ->
					Line = lineParse(T),
					primePid ! Line,
					timePid ! Line,
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

lineParse_privmsg_test() ->
	?assertEqual(["CalebDelnay", "calebd@localhost", "PRIVMSG", "#mychannel", "Hello everyone!"] ,lineParse(":CalebDelnay!calebd@localhost PRIVMSG #mychannel :Hello everyone!")).

lineParse_quit_test() ->
	?assertEqual(["CalebDelnay", "calebd@localhost", "QUIT", "Byte bye!"] ,lineParse(":CalebDelnay!calebd@localhost QUIT :Bye bye!")).

lineParse_ping_test() ->
	?assertEqual(["PING", "irc.localhost.localdomain"] ,lineParse("PING :irc.localhost.localdomain")).

lineParse_mode_test() ->
	?assertEqual(["CalebDelnay", "calebd@localhost", "MODE", "#mychannel", "-l"], lineParse(":CalebDelnay!calebd@localhost MODE #mychannel -l")).
	

lineParse(Str) ->
	From = string:sub_word(string:sub_word(Str, 1, $:), 1, $!),
	Host = string:sub_word(string:sub_word(Str, 2, $!), 1),
	Command = string:sub_word(Str, 2),
	Target = string:sub_word(Str, 3),
	Message = string:strip(string:strip(string:sub_word(Str, 2, $:)), both, $\r),
	[From, Host, Command, Target, Message].
