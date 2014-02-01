-module(ircParser).
-export([parse/1, lineParse/1]).

% starts passing the message around to the different handlers.
parse(SendPid) ->
    receive
    	die ->
    		exit(self(), normal);
		["PING :" ++ T] ->
		    SendPid ! {command, {"PONG", T}};
		T -> 
			Command = string:sub_word(T, 2),
			if 
				Command == "PRIVMSG" ->
					Line = lineParse(T),
					case Line of
						[_,_,_,_,"#q"] ->
							SendPid ! {command, {"QUIT", ":Earl Out!"}};
						[_,_,_,_,"#j"] -> 
							SendPid ! {command, {"JOIN", "#cs"}};
						_Else ->
							false
					end;
				true ->
					checkIndentResponce(re:run(T, "NOTICE AUTH :... Got Ident response"), SendPid)
			end
			%checkQuit(re:run(T, "PRIVMSG Earl2 :#q"), SendPid),
			%checkJoin(re:run(T, "PRIVMSG Earl2 :#j"), T, SendPid)
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
	Message = string:sub_word(Str, 2, $:),
	[From, Host, Command, Target, Message].
