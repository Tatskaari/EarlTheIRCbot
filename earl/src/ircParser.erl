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
			#privmsg{target=Target, from=Nick,  admin=IsAdmin, message=Trail};	     
		"PING" -> #ping{nonce=Trail};
		"MODE" -> #mode{modes=Trail};
		"NOTICE" -> #notice{target=lists:nth(1, Params), message=Trail};

		% MOTD, print it and throw it away %
		"372"  -> #motd{message=Trail};
		
		% Start of MOTD
		"375" -> #raw{data=Str, number_code=Command};
			
		% End of MOTD
		"376" -> #raw{data=Str, number_code=Command};
		%welcome
		"001" -> #raw{data=Str, trail=Trail, number_code=Command};
		%welcome
		"002" -> #raw{data=Str, number_code=Command, trail=Trail};
		%welcome
		"003" -> #raw{data=Str, number_code=Command, trail=Trail};
		%RPL_MYINFO
		"004" ->
			Server_name = lists:nth(2, Params),
			Server_version = lists:nth(3, Params),
			User_modes = lists:nth(4, Params),
			Chan_modes = lists:nth(5, Params),
			settingsServer:setValue(settings, server_name, lists:nth(2, Params)),
			settingsServer:setValue(settings, server_version, lists:nth(3, Params)),
			settingsServer:setValue(settings, user_modes, lists:nth(4, Params)),
			settingsServer:setValue(settings, chan_modes, lists:nth(5, Params)),
			#rpl_myinfo{server_name=Server_name, server_version=Server_version, user_modes=User_modes, chan_modes=Chan_modes};
			%TODO: this in incomplete for some servers

		% Server options
		"005" ->  #raw{data=Str, trail=Trail, number_code=Command};

		% Server users
		% RPL_LUSERCLIENT :There are <int> users and <int> invisible on <int> servers
		"251" -> #raw{data=Str, trail=Trail, number_code=Command};
		% RPL_LUSEROP Number of Ops online <int> :<info>
		"252" -> #raw{data=Str, trail=Trail, number_code=Command};
		% RPL_LUSERCHANNELS Number of Channels formed <int> :<info>
		"254" -> #raw{data=Str, trail=Trail, number_code=Command};
		% RPL_LUSERME Information about local connections; Text may vary.
		"255" -> #raw{data=Str, trail=Trail, number_code=Command};
		"265" -> #raw{data=Str, trail=Trail, number_code=Command};
		"266" -> #raw{data=Str, trail=Trail, number_code=Command};

		% Channel join
		"JOIN" -> 
			storeChanInfo(Trail, name, Trail),
			#join{channel=Trail, nick=Nick};
		% RPL_TOPIC
		"332"  ->
			ChannelName = lists:nth(1, Params),
			storeChanInfo(ChannelName, topic, Trail),
			#rpl_topic{channel=ChannelName, topic=Trail};
		%RPL_TOPICWHOTIME
		%"333"  -> 
		%	Channel = lists:nth(2, Params), %TODO check this
		%	SetBy = lists:nth(3, Params),
		%	Date = msToDate(lists:nth(4, Params)),
		%	#rpl_topicwhotime{channel=Channel, date=Date, nick=SetBy};
		%PL_NAMREPLY
		"353"  -> #raw{data=Str, trail=Trail, number_code=Command};
		"366"  -> #raw{data=Str, trail=Trail, number_code=Command};
	        
	        % Nick
	        "NICK" -> #nick{nick={Nick,Trail}};

		% Part
		"PART" -> #part{nick=Nick, channel=lists:nth(1,Params)};

		% Quits
		"QUIT" -> #quit{reason=Trail, nick=Nick};

		%topic
		"TOPIC" -> 
			Channel = lists:nth(1, Params),
			OldTopic = getChanInfo(Channel, topic),
			storeChanInfo(Channel, topic, Trail),
			#topic{channel=Channel, old_topic=OldTopic, new_topic=Trail, setby=Nick};
		
		% Nick already in use
		"433" ->
			#raw{data=Str, trail=Trail, number_code=Command};


		% Unknown commands
		_A -> #raw{data=Str, trail=Trail, number_code=Command}
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

-ifdef(COLORS). 
% Prints a message in a given colour
print(Catagory, Color, Message, Params) ->
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
	end.
-else.
print(Catagory, _Color, Message, Params) ->
	io:format(Catagory ++ ": " ++ Message, Params).
-endif.
