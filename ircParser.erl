-module(ircParser).
-export([parse/1]).

parse(SendPid) ->
    receive
    	die ->
    		exit(self(), normal);
		["PING :" ++ T] ->
		    SendPid ! {command, {"PONG", T}};
		[T] -> 
			checkQuit(re:run(T, "PRIVMSG Earl2 :#q$"), SendPid)
    end,
    parse(SendPid).
checkQuit({match,[A]}, SendPid) ->
	SendPid ! {command, {"QUIT", "Earl Out!"}};
checkQuit(A, SendPid) ->
	null.
