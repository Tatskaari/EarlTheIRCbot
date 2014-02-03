-module(ircParser).
-export([start/1, parse/1, lineParse/1]).
-import(optimusPrime, [optimusPrime/1]).
-import(time, [time/1]).
<<<<<<< HEAD

=======
-import(telnet, [telnet/1]).
>>>>>>> 2640a81b152c1398d6d89a1fa3f6d099ba4a045c
-include_lib("eunit/include/eunit.hrl").
-include("ircParser.hrl").

start(SendPid) ->
	register(primePid, spawn(optimusPrime, optimusPrime, [SendPid])),
	register(timePid, spawn(time, time, [SendPid])),
	register(telnetPid, spawn(telnet, telnet, [SendPid])),
	parse(SendPid).

% starts passing the message around to the different handlers.
parse(SendPid) ->
    receive
		die ->
			io:format("parserPid :: EXIT~n"),
			primePid ! die,
			timePid ! die,
			telnetPid ! die,
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
					telnetPid ! Line,
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



getPrefix_test() ->
	?assertEqual({true, "a", "b"}, getPrefix(":a b")),
	?assertEqual({true, "a", "b"}, getPrefix(":a     b")),
	?assertEqual({false, "", "b"}, getPrefix("b")).

getPrefix(":" ++ Str) ->
	SpaceIndex = string:str(Str, " "),
	Prefix = string:substr(Str, 1, SpaceIndex-1),
	Rest = string:strip(string:substr(Str, SpaceIndex), left),
	{true, Prefix, Rest};
getPrefix(Str) -> {false, "", Str}.


getTrail_test() ->
	{true, _, Rest} = getPrefix(":Mex!~a@a.kent.ac.uk PRIVMSG #bottesting : :"),
	?assertEqual({true, " :", "PRIVMSG #bottesting"}, getTrail(Rest)),
	{true, _, Rest} = getPrefix(":Mex!~a@a.kent.ac.uk PRIVMSG #bottesting : :"),
	?assertEqual({true, " :", "PRIVMSG #bottesting"}, getTrail(Rest)).

getTrail(Str) ->
	Index = string:str(Str, " :"),
	case Index of
		0 -> {false, "", Str};
		_ ->
			io:format("Index: ~p~n", [Index]),
			Rest = string:strip(string:substr(Str, 1, Index)),
			Trail = string:substr(Str, Index + 2),
			{true, Trail, Rest}
	end.

getCommand_test() ->
	?assertEqual({"PRIVMSG", ["#bottesting"]}, getCommand("PRIVMSG #bottesting")).

getCommand(Str) ->
	Tokens = string:tokens(Str, " "),
	[Command|Params] = Tokens,
	{Command, Params}.

lineParse_privmsg_test() ->
	?assertEqual(#privmsg{message="Hello everyone!", target="#mychannel"} ,lineParse2(":CalebDelnay!calebd@localhost PRIVMSG #mychannel :Hello everyone!")),
	?assertEqual(#privmsg{message=":", target="#bottesting"}, lineParse2(":Mex!~a@a.kent.ac.uk PRIVMSG #bottesting ::")).

%lineParse_quit_test() ->
%	?assertEqual(["CalebDelnay", "calebd@localhost", "QUIT", "Byte bye!"] ,lineParse(":CalebDelnay!calebd@localhost QUIT :Bye bye!")).
%
lineParse_ping_test() ->
	?assertEqual(#ping{nonce="irc.localhost.localdomain"} ,lineParse2("PING :irc.localhost.localdomain")).
%
%lineParse_mode_test() ->
%	?assertEqual(["CalebDelnay", "calebd@localhost", "MODE", "#mychannel", "-l"], lineParse(":CalebDelnay!calebd@localhost MODE #mychannel -l")).

lineParse2(Str) ->
	{HasPrefix, Prefix, Rest} = getPrefix(Str),
	{HasTrail, Trail, CommandsAndParams} = getTrail(Rest),
	{Command, Params} = getCommand(CommandsAndParams),
	case Command of
		"PRIVMSG" -> #privmsg{target=lists:nth(1, Params), message=Trail};
		"PING" -> #ping{nonce=Trail}
	end.



lineParse(Str) ->
	NumberOfWords = string:words(Str),
	if 
		NumberOfWords < 3  ->
			parseActionServerCmd(Str);
		NumberOfWords > 2 ->
			parseUserMessage(Str)
	end.



parseActionServerCmd(Str) ->
	Command = string:sub_word(Str, 1),
	Data = string:sub_word(Str, 2, $:),
	{server_command, {Command, Data}}.

parseUserMessage(Str) ->
	Commands = string:sub_word(Str, 1, $:),
	
	User = string:sub_word(Commands, 1, $!),
	Host = string:sub_word(string:sub_word(Commands, 2, $!), 1),
	Command = sting:sub_word(Str, 2),
	CommandsLength = string:len(Commands),

	Message = string:substr(Str, CommandsLength + 2),

	CommandsLength = string:words(Commands),
	if
		CommandsLength == 3 ->
			% Targeted message
			Target = Command = sting:sub_word(Str, 3),
			{message, {User, Host, Command, Target, Message}};
		CommandsLength == 2 ->
			% Non-targetted message
			{message, {User, Host, Command, Message}}
	end,
	{error, {}}.
	

%	From = string:sub_word(string:sub_word(Str, 1, $:), 1, $!),
%	Host = string:sub_word(string:sub_word(Str, 2, $!), 1),
%	Command = string:sub_word(Str, 2),
%	Target = string:sub_word(Str, 3),
%	Message = string:strip(string:strip(string:sub_word(Str, 2, $:)), both, $\r),
%	[From, Host, Command, Target, Message].
