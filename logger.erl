-module(logger).
-behaviour(gen_event).
-include("ircParser.hrl").
-include("earl.hrl").
-export([init/1, handle_event/2, terminate/2]).
-export([code_change/3, handle_info/2]). 

init(_Args) ->
	{ok, []}.

handle_event(#privmsg{target=Target, from=From, message=Message}, State) ->
	print("PRIVMSG", blue, "[~s -> ~s]: ~s~n", [From, Target, Message]),
	{ok, State};

handle_event(#notice{target=_Target, message=Message}, State) ->
	print("NOTICE", blue, "~s~n", [Message]),
	{ok, State};

handle_event(#motd{message=Message}, State) ->
	print("MOTD", green, "~s~n", [Message]),
	{ok, State};

handle_event(#topic{channel=Channel, old_topic=OldTopic, new_topic=NewTopic, setby=Nick}, State) ->
	print("TOPIC", green, "~s set topic of ~s to '~s'", [Nick, Channel, NewTopic]),
	{ok, State};

handle_event(#raw{data=Data, numbercode=NumberCode, trail=Trail}, State) ->
	case NumberCode of
		"001" -> print("INFO(001)", blue, "~s~n", [Trail]);
		"002" -> print("INFO(002)", blue, "~s~n", [Trail]);
		"003" -> print("INFO(003)", blue, "~s~n", [Trail]);
		"005" -> print("SERV(005)", green, "~s~n", [Trail]);
		"251" -> print("USERS(251)", green, "~s~n", [Trail]);
		"252" -> print("USERS(252)", green, "~s~n", [Trail]);
		"254" -> print("USERS(254)", green, "~s~n", [Trail]);
		"255" -> print("USERS(255)", green, "~s~n", [Trail]);
		"265" -> print("USERS(265)", green, "~s~n", [Trail]);
		"266" -> print("USERS(266)", green, "~s~n", [Trail]);
		"353" -> print("JOIN", green, "Users: ~s~n", [Trail]);
		"366" -> print("JOIN", green, "End of users list~n", []);
		_Other -> print("WARNING", yellow, "Unknown number code '~s'~n", [NumberCode]);
	end,
	{ok, State};

handle_event(#rpl_myinfo{server_name=Server_name, server_version=Server_version, user_modes=User_modes, chan_modes=Chan_modes}, State) ->
	print("INFO(004)", blue, "Server Name is '~s'~n", [Server_name]),
	print("INFO(004)", blue, "Server Version is '~s'~n", [Server_version]),
	print("INFO(004)", blue, "User Modes [~s]~n", [User_modes]),
	print("INFO(004)", blue, "Channel Modes[~s]~n", [Chan_modes]),
	{ok, State};

handle_event(#join{channel=Channel, nick=Nick}, State) ->
	print("JOIN", green, "~s joined ~s~n", [Nick, Channel]),
	{ok, State};

handle_event(#rpl_topic{channel=Channel, topic=Topic}, State) ->
	print("JOIN", green, "Topic: ~s~n", [Topic]),
	{ok, State};

handle_event(#rpl_topicwhotime{channel=Channel, date=Date, nick=Nick}, State) ->
	print("JOIN", green, "Topic of ~n set by ~s at ~s~n", [Channel, Nick, Date]),
	{ok, State};

handle_event(#nick{nick={Old, New}}, Status) ->
	print("NICK", blue, "~s changed to ~s~n", [Old, New]),
	{ok, State};

handle_event(#part{nick=Nick, channel=Channel) ->
	print("PART", green, "~s parted ~s~n", [Nick, Channel]),
	{ok, State};

handle_event(_Msg, State) ->
	{ok, State}.

terminate(_Args, _State) ->
    ok.


handle_info({'EXIT', _Pid, _Reason}, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


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
