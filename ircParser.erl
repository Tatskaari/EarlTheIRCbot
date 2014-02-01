-module(ircParser).
-export([parse/1]).

parse(SendPid) ->
    receive
    	die ->
    		exit(self(), normal);
		["PING :" ++ T] ->
		    SendPid ! {command, {"PONG", T}};
		[T] -> 
			checkQuit(re:run(T, "PRIVMSG Earl2 :#q"), SendPid),
			checkJoin(re:run(T, "PRIVMSG Earl2 :#j"), T, SendPid)
    end,
    parse(SendPid).

checkQuit({match,[A]}, SendPid) ->
	SendPid ! {command, {"QUIT", "Earl Out!"}},
	true;
checkQuit(_, _) ->
	flase.

checkJoin({match, [{Start, Length}]}, Message, SendPid) ->
	Channel = string:sub_string(Message, Start+Length+2),
	SendPid ! {command, {"JOIN", Channel}},
	true;
checkJoin(_, _, _) ->
	false.
