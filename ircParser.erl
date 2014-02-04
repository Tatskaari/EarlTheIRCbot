-module(ircParser).
-export([parse/0, lineParse/1]).
-include_lib("eunit/include/eunit.hrl").

-define(NICK, "mextest").
-define(USER, "Tatskaari Sir_Earl Sir_Earl Sir_Earl").

%Contains the record definitions
-include("ircParser.hrl").

%Include Tests
-include("ircParser_test.erl").

% Starts passing the message around to the different handlers.
parse() ->
    receive
		die ->
			io:format("parserPid :: EXIT~n"),
			primePid ! die,
			timerPid ! die,
			telnetPid ! die,
			exit(self(), normal);
		T->
			Line = lineParse(T),

			% Commands which don't need admin
			case Line of
				% Join (#j)
				#privmsg{admin=true, message="#j " ++ K} ->
					sendPid ! {command, {"JOIN", K}};
				
				% Quit (#q [reason])
				#privmsg{admin=true, message="#q " ++ K} ->
					sendPid ! {command, {"QUIT", ":" ++ K}};
				#privmsg{admin=true, message="#q"} ->
					sendPid ! {command, {"QUIT", ":Earl out"}};

				% Is Prime Number (#isPrime <num>)
				#privmsg{message="#isPrime" ++ _K} ->
					primePid ! Line;

				% List the primes to a given number (#primesTo <num>)
				#privmsg{message="#primesTo " ++ _K} ->
					primePid ! Line;

				% Time (#t)
				#privmsg{message="#t"} ->
					timerPid ! Line;

				% Telnet (#telnet)
				#privmsg{message="#telnet " ++ _} ->
					telnetPid ! Line;

				% Ping
				#ping{nonce=K} ->
					sendPid ! {command, {"PONG", K}};

				_Default -> false % We don't know about everything - let's not deal with it.
			end,
		checkIndentResponce(re:run(T, "NOTICE AUTH :... Got Ident response"))
    end,
    parse().


% Connects to the server after indent response [[ NEEDS REDOING ]]
checkIndentResponce({match, [_]}) ->
	sendPid ! {command, {"USER", ?USER}},
	sendPid ! {command, {"NICK", ?NICK}},
	true;
checkIndentResponce(_) ->
	false.


% Get the command part of a line
getPrefix(":" ++ Str) ->
	SpaceIndex = string:str(Str, " "),
	Prefix = string:substr(Str, 1, SpaceIndex-1),
	Rest = string:strip(string:substr(Str, SpaceIndex), left),
	{true, Prefix, Rest};
getPrefix(Str) -> {false, "", Str}.


% Get the tail of a given string (the message part)
getTrail(Str) ->
	Index = string:str(Str, " :"),
	case Index of
		0 -> {false, "", Str};
		_ ->
			% io:format("Index: ~p~n", [Index]),
			Rest = string:strip(string:substr(Str, 1, Index)),
			Trail = string:strip(string:strip(string:substr(Str, Index + 2), both, $\n), both, $\r),
			{true, Trail, Rest}
	end.


% Get the command from a given string
getCommand(Str) ->
	Tokens = string:tokens(Str, " "),
	[Command|Params] = Tokens,
	{Command, Params}.


% Get the nick part of a user!host string
getNick(Str) ->
	string:sub_word(Str, 1, $!).


% Parse a line
lineParse(Str) ->
	{_HasPrefix, Prefix, Rest} = getPrefix(Str),
	{_HasTrail, Trail, CommandsAndParams} = getTrail(Rest),
	{Command, Params} = getCommand(CommandsAndParams),
	Nick = getNick(Prefix),
	IsAdmin = isAdmin(Nick, ["graymalkin", "Tatskaari", "Mex", "xand", "Tim"]),
	case Command of
		"PRIVMSG" -> #privmsg{target=lists:nth(1, Params), from=getNick(Prefix),  admin=IsAdmin, message=Trail};
		"PING" -> #ping{nonce=Trail};
		% We don't know about everything - let's not deal with it.
		A -> io:format("WARNING: Un-recognised command '~s': '~s'~n", [Command, Str])
	end.


% Checks that a list contains a given string
isAdmin(_, []) -> false;
isAdmin(Str, List) ->
	[Head|Tail] = List,
	if
		Head == Str ->
			true;
		true ->
			isAdmin(Str, Tail)
	end.
	


