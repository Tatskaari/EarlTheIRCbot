-module(ircParser).
-export([parse/0, parse/1, lineParse/1]).
-include_lib("eunit/include/eunit.hrl").

-include("earl.hrl").

-define(NICK, "MexEarl").
-define(USER, "Tatskaari Sir_Earl Sir_Earl Sir_Earl").

%Contains the record definitions
-include("ircParser.hrl").

%Include Tests
-include("ircParser_test.erl").

% Starts passing the message around to the different handlers.
parse() ->
	parse([]).

parse(PluginsChans) ->
    receive
		die ->
			io:format("parserPid :: EXIT~n"),
			primePid ! die,
			timerPid ! die,
			telnetPid ! die,
			exit(self(), normal);
	    	#registerPlugin{chan=Pid} ->
		    ?MODULE:parse([Pid|PluginsChans]);

		T->
			Line = lineParse(T),
			case Line of
				{} -> {};
				_A ->

				F = fun(Chan) -> Chan ! Line end,
				lists:foreach(F, PluginsChans),
				% Commands which don't need admin
				case Line of
					% Ping
					#ping{nonce=K} ->
						sendPid ! {command, {"PONG", K}};

					_Default -> false % We don't know about everything - let's not deal with it.
				end
			end,
		checkIndentResponce(re:run(T, "NOTICE AUTH :... Got Ident response"))
    end,
    ?MODULE:parse(PluginsChans).


% Connects to the server after indent response [[ NEEDS REDOING ]]
checkIndentResponce({match, [_]}) ->
	sendPid ! {command, {"USER", ?USER}},
	sendPid ! {command, {"NICK", ?NICK}},
	sendPid ! {command, {"JOIN", "#bottesting"}},
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
		"MODE" -> #mode{modes=Trail};
		"NOTICE" -> 
			io:format("NOTICE: ~s~n", [Trail]),
			#notice{target=lists:nth(1, Params), message=Trail};
		%MOTD, print it and throw it away %
		"372"  -> io:format("MOTD: ~s~n", [Trail]), {};
		%start of MOTD
		"375" -> {};
		%end of MOTD
		"376" -> {};
		%welcome
		"001" -> io:format("INFO: ~s~n", [Trail]), {};
		%welcome
		"002" -> io:format("INFO: ~s~n", [Trail]), {};
		%welcome
		"003" -> io:format("INFO: ~s~n", [Trail]), {};
		%RPL_MYINFO
		"004" ->
			settings ! #setVal{name=server_name, value=lists:nth(2, Params)},
			settings ! #setVal{name=server_version, value=lists:nth(3, Params)},
			settings ! #setVal{name=user_modes, value=lists:nth(4, Params)},
			settings ! #setVal{name=chan_modes, value=lists:nth(5, Params)},
			{};
			%TODO: this in incomplete for some servers
		% We don't know about everything - let's not deal with it.
		A -> io:format("WARNING: Un-recognised command '~s': '~s'~n", [Command, Str]),{}
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
	


