-module(ircParser).
-export([parse/0, parse/1, lineParse/1]).
-include_lib("eunit/include/eunit.hrl").

-include("earl.hrl").
-define(NICK, "SimonsEarl").
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
					% Anonnomous function (F) to send line to every registered plugin
					F = fun(Chan) -> Chan ! Line end,
					% For each plugin run F against it
					lists:foreach(F, PluginsChans),

					% Built in commands which are required for the protocol
					case Line of
						% Ping
						#ping{nonce=K} ->
							sendPid ! #pong{nonce=K};

						#privmsg{from=From, target=To, message="#plugins" ++ _} ->
							io:format("~p~p~n~p~n", [To, From, Line]),
							ListPlugins = fun(Chan) ->
								M = io_lib:format("~p", [Chan]),
								sendPid ! #privmsg{target=To, message=("Plugin: " ++ M)}
							end,
							lists:foreach(ListPlugins, PluginsChans);

						% We don't know about everything - let's not deal with it.	
						_Default -> false 
					end
			end,
		checkIndentResponce(re:run(T, "NOTICE AUTH :... Got Ident response"))
    end,
    ?MODULE:parse(PluginsChans).


% Connects to the server after indent response [[ NEEDS REDOING ]]
checkIndentResponce({match, [_]}) ->
	sendPid ! #user{user=?USER},
	sendPid ! #nick{nick=?NICK},
	true;
checkIndentResponce(_) ->
	false.


% Get the command part of a line
% Produces tuple: {HasPrefix, Prefix, Rest}
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


getAdmins() ->
	["graymalkin", "Tatskaari", "Mex", "xand", "Tim"].
%	case dict:is_key(admins, Dict) of
%		true ->
%			dict:fetch(admins, Dict);
%		false ->
%			false
%	end.
%	settings ! #getVal{name=admins, return_chan=getAdmins()},
%	receive
%		A -> A
%	end,
%	getAdmins([]).


% Parse a line
lineParse(Str) ->
	{_HasPrefix, Prefix, Rest} = getPrefix(Str),
	{_HasTrail, Trail, CommandsAndParams} = getTrail(Rest),
	{Command, Params} = getCommand(CommandsAndParams),
	Nick = getNick(Prefix),
	IsAdmin = isAdmin(Nick, getAdmins()),

	case Command of
		"PRIVMSG" -> #privmsg{target=lists:nth(1, Params), from=Nick,  admin=IsAdmin, message=Trail};
		"PING" -> #ping{nonce=Trail};
		"MODE" -> #mode{modes=Trail};
		"NOTICE" -> 
			io:format("NOTICE: ~s~n", [Trail]),
			#notice{target=lists:nth(1, Params), message=Trail};

		% MOTD, print it and throw it away %
		"372"  -> io:format("MOTD: ~s~n", [Trail]), {};
		
		% Start of MOTD
		"375" -> {};
		
		% End of MOTD
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

		% Server options
		"005" -> io:format("SERV: ~s~n", [Trail]), {};

		% Server users
		"251" -> io:format("USERS: ~s~n", [Trail]), {};
		"252" -> io:format("USERS: ~s~n", [Trail]), {};
		"254" -> io:format("USERS: ~s~n", [Trail]), {};
		"255" -> io:format("USERS: ~s~n", [Trail]), {};
		"265" -> io:format("USERS: ~s~n", [Trail]), {};
		"266" -> io:format("USERS: ~s~n", [Trail]), {};

		% Channel join
		"JOIN" -> io:format("JOIN: ~s joined ~s~n", [Nick, Trail]);
		"332"  -> io:format("JOIN: Topic: ~s~n", [Trail]);
		"333"  -> io:format("JOIN: ~s~n", [lists:nth(1, Params)]);
		"353"  -> io:format("JOIN: Users: ~s~n", [Trail]);
		"366"  -> io:format("JOIN: End of users list.~n");

		% Part
		"PART" -> io:format("PART: ~s parted ~s~n", [Nick, lists:nth(1, Params)]);

		% Quits
		"QUIT" -> io:format("QUIT: ~s quit (~s)~n", [Nick, Trail]);
		
		% Nick already in use
		"433" -> io:format("ERROR: Nick already in use."), {};

		"436" -> io:format("ERROR: Nick collision."), {};

		% Unknown commands
		_A -> io:format("WARNING: Un-recognised command '~s': '~s'~n", [Command, Str]),{}
	end.
