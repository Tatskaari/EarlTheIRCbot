-module(ircParser).
-export([lineParse/1, print/4, getCommand/1, getTrail/1, getPrefix/1, isAdmin/2]).
-include_lib("eunit/include/eunit.hrl").
-import(settingsServer, [getSetting/2]).
-import(optimusPrime, [optimusPrime/0]).
-import(time, [timer/0]).
-import(telnet, [telnet/0]).
-import(earlAdminPlugin, [earlAdminPlugin/0]).
-import(reminder, [reminder/0]).
-include("earl.hrl").

%Contains the record definitions
-include("ircParser.hrl").


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
	getSetting(settings, admins).


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
			print("NOTICE", blue, "~s~n", [Trail]),
			#notice{target=lists:nth(1, Params), message=Trail};

		% MOTD, print it and throw it away %
		"372"  -> print("MOTD", green, "~s~n", [Trail]), {};
		
		% Start of MOTD
		"375" -> {};
			
		% End of MOTD
		"376" -> {};
		%welcome
		"001" -> print("INFO", blue, "~s~n", [Trail]), {};
		%welcome
		"002" -> print("INFO", blue, "~s~n", [Trail]), {};
		%welcome
		"003" -> print("INFO", blue, "~s~n", [Trail]), {};
		%RPL_MYINFO
		"004" ->
			settings ! #setVal{name=server_name, value=lists:nth(2, Params)},
			settings ! #setVal{name=server_version, value=lists:nth(3, Params)},
			settings ! #setVal{name=user_modes, value=lists:nth(4, Params)},
			settings ! #setVal{name=chan_modes, value=lists:nth(5, Params)},
			{};
			%TODO: this in incomplete for some servers

		% Server options
		"005" -> print("SERV", green, "~s~n", [Trail]), {};

		% Server users
		"251" -> print("USERS", green, "~s~n", [Trail]), {};
		"252" -> print("USERS", green, "~s~n", [Trail]), {};
		"254" -> print("USERS", green, "~s~n", [Trail]), {};
		"255" -> print("USERS", green, "~s~n", [Trail]), {};
		"265" -> print("USERS", green, "~s~n", [Trail]), {};
		"266" -> print("USERS", green, "~s~n", [Trail]), {};

		% Channel join
		"JOIN" -> 
			print("JOIN", green, "~s joined ~s~n", [Nick, Trail]),
			case getSetting(channel_info, Trail) of
				#retVal{name=Trail, value=X} ->
					%x should hold a setting server for this chan, update it's value.
					X ! #setVal{name=name, value=Trail},
					{};
				#noVal{name=Trail} ->
					% we better start a settings server to hold details about this chan
					NewSettingServer = spawn(settingsServer, setting_server, []),
					channel_info ! #setVal{name=Trail, value=NewSettingServer},
					NewSettingServer ! #setVal{name=name, value=Trail},
					{}
			end;
		"332"  ->
			print("JOIN", green, "Topic: ~s~n", [Trail]), {},
			ChannelName = lists:nth(1, Params),
			case getSetting(channel_info, ChannelName) of
				#retVal{name=ChannelName, value=X} ->
					% X should hold a setting server for this chan, update it's value.
					X ! #setVal{name=topic, value=Trail},
					{};
				#noVal{name=ChannelName} ->
					% we better start a settings server to hold details about this chan
					NewSettingServer = spawn(settingsServer, setting_server, []),
					channel_info ! #setVal{name=ChannelName, value=NewSettingServer},
					NewSettingServer ! #setVal{name=topic, value=Trail},
					{}
			end;
		"333"  -> print("JOIN", green, "Topic set by ~s at ~s~n", [lists:nth(3, Params), msToDate(lists:nth(4, Params)) ]), {};
		"353"  -> print("JOIN", green, "Users: ~s~n", [Trail]), {};
		"366"  -> print("JOIN", green, "End of users list~n", []), {};
	        
        % Nick
        "NICK" -> print("NICK", blue, "~s changed to ~s~n", [Nick, Trail]),
			#nick{nick={Nick,Trail}};

		% Part
		"PART" -> print("PART", green, "~s parted ~s~n", [Nick, lists:nth(1, Params)]), {};

		% Quits
		"QUIT" -> print("QUIT", green, "~s quit (~s)~n", [Nick, Trail]), {};
		
		% Nick already in use
		"433" -> print("ERROR", red, "Nick already in use.~n", []), {};

		"436" -> print("ERROR", red, "Nick collision.~n", []), {};

		% Error
		"ERROR" -> print("ERROR", red, "~s~n", [Trail]);

		% Unknown commands
		_A -> print("WARN", yellow, "Un-recognised command '~s': '~s'~n", [Command, Str]), {}
	end.

% Thanks StackOverflow! http://stackoverflow.com/questions/825151/convert-timestamp-to-datetime-in-erlang
msToDate(Str) ->
	{ InS, _Rest } = string:to_integer(Str), 
	BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
	Seconds       = BaseDate + InS,
	{Date, Time}  = calendar:gregorian_seconds_to_datetime(Seconds),
	ircTime:date_to_string({Date, Time}).

 
% Prints a message in a given colour
print(Catagory, Color, Message, Params) when ?COLORS ->
	case Color of
		red ->
			io:format("\e[0;31m" ++ Catagory ++ "\e[0;37m" ++ ": " ++ Message, Params);

		green ->
			io:format("\e[0;32m" ++ Catagory ++ "\e[0;37m" ++ ": " ++ Message, Params);

		yellow ->
			io:format("\e[0;33m" ++ Catagory ++ "\e[0;37m" ++ ": " ++ Message, Params);

		blue ->
			io:format("\e[0;36m" ++ Catagory ++ "\e[0;37m" ++ ": " ++ Message, Params);

		_Default ->
			io:format(Catagory ++ ": " ++ Message, Params)
	end;
print(Catagory, _Color, Message, Params) ->
	io:format(Catagory ++ ": " ++ Message, Params).
