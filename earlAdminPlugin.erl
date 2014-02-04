-module(earlAdminPlugin).
-include("ircParser.hrl").
-export([start/1]).

start(SendPid) ->
	receive
		% nick (#n <NICK>)
		#privmsg{admin=true, message="#n " ++ Nick} ->
			SendPid ! #nick{nick=?NICK};

		% Join (#j <CHANNEL>)
		#privmsg{admin=true, message="#j " ++ K} ->
			io:format("Joining '~s'~n", [K]),
			SendPid ! #join{channel=K};
		
		% Part (#p <CHANNEL>)
		#privmsg{admin=true, message="#p " ++ Channel} ->
			SendPid ! #part{channel=Channel};

		% Quit (#q [reason])
		#privmsg{admin=true, message="#q " ++ K} ->
			SendPid ! #quit{reason=K};
		#privmsg{admin=true, message="#q"} ->
			SendPid ! #quit{reason="Earl Out"};

		Default -> false % We don't know about everything - let's not deal with it.
	end,
	?MODULE:start(SendPid).
