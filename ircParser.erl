-module(ircParser).
-export([parse/1]).

% starts passing the message around to the different handlers.
parse(SendPid) ->
    receive
    	die ->
    		exit(self(), normal);
		["PING :" ++ T] ->
		    SendPid ! {command, {"PONG", T}};
		T -> 
			checkIndentResponce(re:run(T, "NOTICE AUTH :... Got Ident response"), SendPid),
			checkQuit(re:run(T, "PRIVMSG Earl2 :#q"), SendPid),
			checkJoin(re:run(T, "PRIVMSG Earl2 :#j"), T, SendPid)
    end,
    parse(SendPid).

% connects tot he server after indent responce
checkIndentResponce({match, [_]}, SendPid) ->
	SendPid ! {command, {"USER", "Sir_Earl Sir_Earl Sir_Earl Sir_Earl"}},
	SendPid ! {command, {"NICK", "Earl2"}},
	true;
checkIndentResponce(_,_) ->
	false.

% if the command is quit, then quit
checkQuit({match,[_]}, SendPid) ->
	SendPid ! {command, {"QUIT", "Earl-Out!"}},
	true;
checkQuit(_, _) ->
	flase.

% if the command is join, then join
checkJoin({match, [{Start, Length}]}, Message, SendPid) ->
	Channel = string:sub_string(Message, Start+Length+2),
	SendPid ! {command, {"JOIN", Channel}},
	true;
checkJoin(_, _, _) ->
	false.
