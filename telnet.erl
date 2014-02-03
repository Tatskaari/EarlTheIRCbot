-module(telnet).
-export([telnet/1]).

%Contains the record definitions
-include("ircParser.hrl").

telnet(SendPid) ->
	receive
		#privmsg{target=Target, from=From, message="#telnet " ++ K}->
			{Host, Port} = parser(K),
			SendPid ! {prvmsg, {From, Target, "Host: " ++ Host ++ " Port: " + Port}};
		die ->
			io:format("telnetPid :: EXIT~n")
	end,
	telnet(SendPid).

% will eventually parse the message and pass it to the right thread to do things 
parser(K) ->
	string:tokens(K, " ").



