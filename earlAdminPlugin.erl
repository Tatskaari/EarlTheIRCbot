-module(earlAdminPlugin).
-include("ircParser.hrl").
-export([start/1]).

start(SendPid) ->
	receive
		% nick (#n <NICK>)
		#privmsg{admin=true, message="#n " ++ Nick} ->
			SendPid ! {command, {"NICK", Nick}};

		% Join (#j <CHANNEL>)
		#privmsg{admin=true, message="#j " ++ K} ->
			io:format("Joing '~s'~n", [K]),
			SendPid ! {command, {"JOIN", K}};
		
		% Part (#p <CHANNEL>)
		#privmsg{admin=true, message="#p " ++ Channel} ->
			SendPid ! {command, {"PART", Channel}};

		% Quit (#q [reason])
		#privmsg{admin=true, message="#q " ++ K} ->
			SendPid ! {command, {"QUIT", ":" ++ K}};
		#privmsg{admin=true, message="#q"} ->
			SendPid ! {command, {"QUIT", ":Earl out"}};

		Default -> false % We don't know about everything - let's not deal with it.
	end,
	?MODULE:start(SendPid).
