-module(telnet).
-export([telnet/0, connect/2, stringToInt/1]).

%Contains the record definitions
-include("ircParser.hrl").

telnet() ->
	receive
		#privmsg{target="#" ++ Target, from=From} ->
			sendPid ! {prvmsg, {From, "#" ++ Target, From ++ ": Please use private chat for telnet."}};
		#privmsg{target=Target, from=From, message="#telnet " ++ K} ->
			case string:tokens(K, " ") of
				[Host, Port] ->
					connect(Host, stringToInt(Port));
				_ ->
					noMatch
			end;
		{tcp, _, A} ->
			sendPid ! {command, {"PRIVMSG", "Tatskaari", A}};
		die ->
			io:format("telnetPid :: EXIT~n"),
			exit(self(), normal)
	end,
	telnet().

% converts a list of ints into the integer they represent 
stringToInt(Str) ->
	case string:to_integer(Str) of
        {error, _} -> -1;
        {F,_Rest} -> F
    end.


% Keeps a list of pids to message
pidList(PidList) ->
	NewPid = receive
		{add, Pid} ->
			pidList(PidList ++ Pid);
		{remove, Pid} ->
			pidList(PidList -- Pid);
		{getList, Pid} ->
			Pid ! PidList;
		die ->
			exit(self(), normal)
	end.

%%%%%%% potential useful code %%%%%%%


connect(Host, Port) ->
	connect(gen_tcp:connect(Host, Port, [], 1000)).
connect({ok, Socket})->
	Socket;
connect({error, Reason}) ->
	io:format("~s~n", [Reason]).
	

% will eventually parse the message and pass it to the right thread to do things 
parser(K) ->
	[Host, Port] = string:tokens(K, " "),
	[Host, Port].

% takes a string and turns it into an integer
listToNum(List) ->
    case string:to_integer(List) of
        {error, _} -> -1;
        {F,_Rest} -> F
    end.



