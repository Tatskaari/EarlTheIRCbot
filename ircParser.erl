-module(ircParser).
-export([lineParse/1, print/4, getCommand/1, getTrail/1, getPrefix/1, isAdmin/2]).
-include_lib("eunit/include/eunit.hrl").
-import(settingsServer, [getValue/2, setValue/3, start_link/0]).
-include("earl.hrl").

%Contains the record definitions
-include("ircParser.hrl").

%Include Tests
%-include("ircParser_test.erl").

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
	settingsServer:getValue(settings, admins).

% Parse a line
lineParse(Str) ->
	{_HasPrefix, Prefix, Rest} = getPrefix(Str),
	{_HasTrail, Trail, CommandsAndParams} = getTrail(Rest),
	{Command, Params} = getCommand(CommandsAndParams),
	Nick = getNick(Prefix),
	IsAdmin = isAdmin(Nick, getAdmins()),

	case Command of
		"PRIVMSG" ->
			Target = lists:nth(1, Params),	
			print("PRIVMSG", blue, "[~s -> ~s]: ~s~n", [Nick, Target, Trail]),
			#privmsg{target=lists:nth(1, Params), from=Nick,  admin=IsAdmin, message=Trail};
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
		"001" -> print("INFO(001)", blue, "~s~n", [Trail]), {};
		%welcome
		"002" -> print("INFO(002)", blue, "~s~n", [Trail]), {};
		%welcome
		"003" -> print("INFO(003)", blue, "~s~n", [Trail]), {};
		%RPL_MYINFO
		"004" ->
			print("INFO(004)", blue, "~s~n", [CommandsAndParams]),
			settingsServer:setValue(settings, server_name, lists:nth(2, Params)),
			settingsServer:setValue(settings, server_version, lists:nth(3, Params)),
			settingsServer:setValue(settings, user_modes, lists:nth(4, Params)),
			settingsServer:setValue(settings, chan_modes, lists:nth(5, Params)),
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
			storeChanInfo(Trail, name, Trail),
			{};
		"332"  ->
			print("JOIN", green, "Topic: ~s~n", [Trail]), 
			ChannelName = lists:nth(1, Params),
			storeChanInfo(ChannelName, topic, Trail),
			{};
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

		%topic
		"TOPIC" -> 
			Channel = lists:nth(1, Params),
			OldTopic = getChanInfo(Channel, topic),
			print("TOPIC", green, "~s set topic of ~s to '~s'", [Nick, Channel, Trail]),
			storeChanInfo(Channel, topic, Trail),
			#topic{channel=Channel, old_topic=OldTopic, new_topic=Trail, setby=Nick};
		
		% Nick already in use
		"433" -> print("ERROR", red, "Nick already in use.", []), {};

		"436" -> print("ERROR", red, "Nick collision.", []), {};

		% Unknown commands
		_A -> print("WARN", yellow, "Un-recognised command '~s': '~s'~n", [Command, Str]), {}
	end.

storeChanInfo(ChannelName, Param, Data) ->
	ChanServer = settingsServer:getValue(channel_info, ChannelName),
	case ChanServer of
		undef ->
			case settingsServer:start_link() of
				{ok, NewServer} ->
					settingsServer:setValue(channel_info, ChannelName, NewServer),
					settingsServer:setValue(NewServer, Param, Data);
				_Else ->
					print("WARNING", red, "Couldn't create serrtingsServer for channel '~s'", [ChannelName])
			end;
		X ->
			settingsServer:setValue(X, Param, Data)
	end.

getChanInfo(ChannelName, Param) ->
	case settingsServer:getValue(channel_info, ChannelName) of
		undef -> {false, undef};
		X ->
			{true, settingsServer:getValue(X, Param)}
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
