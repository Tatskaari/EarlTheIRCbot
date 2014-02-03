-module(telnet).
-export([telnet/1]).

telnet(SendPid) ->
	receive
		[From,_,_,Target,"#telnet " ++ K] ->
			SendPid ! {prvmsg, {From, Target, K}};
		die ->
			io:format("telnetPid :: EXIT~n")
	end,
	telnet(SendPid).