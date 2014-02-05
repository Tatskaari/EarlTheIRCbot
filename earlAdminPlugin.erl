-module(earlAdminPlugin).
-include("ircParser.hrl").
-export([earlAdminPlugin/0]).

earlAdminPlugin() ->
	receive
		% nick (#n <NICK>)
		#privmsg{admin=true, message="#n " ++ Nick} ->
			sendPid ! #nick{nick=Nick};

		% Join (#j <CHANNEL>)
		#privmsg{admin=true, message="#j " ++ K} ->
			io:format("Joining '~s'~n", [K]),
			sendPid ! #join{channel=K};
		
		% Part (#p <CHANNEL>)
		#privmsg{admin=true, message="#p " ++ Channel} ->
			sendPid ! #part{channel=Channel};

		% Quit (#q [reason])
		#privmsg{admin=true, message="#q " ++ K} ->
			sendPid ! #quit{reason=K};
		#privmsg{admin=true, message="#q"} ->
			sendPid ! #quit{reason="Earl Out"};

		% Unloads modules
		#privmsg{admin=true, from=From, target=To, message="#unload " ++ ModuleName} ->
			parserPid ! #deregisterPlugin{name=ModuleName},
			sendPid ! #privmsg{from=From, target=To, message=From ++ ": Unloaded " ++ ModuleName};

		% Loads modules
		#privmsg{admin=true, from=From, target=To, message="#load " ++ ModuleName} ->
			parserPid ! #registerPlugin{name=ModuleName},
			sendPid ! #privmsg{from=From, target=To, message=From ++ ": loaded " ++ ModuleName};

		_Default -> false % We don't know about everything - let's not deal with it.
	end,
	?MODULE:earlAdminPlugin().
