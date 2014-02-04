-module(telnet).
-export([telnet/0]).

%Contains the record definitions
-include("ircParser.hrl").

telnet() ->
	receive
		#privmsg{target="#" ++ Target, from=From} ->
			sendPid ! {prvmsg, {From, "#" ++ Target, From ++ ": Please use private chat for telnet."}};
		#privmsg{target=Target, from=From, message="#telnet " ++ K} ->
			lol;
		die ->
			io:format("telnetPid :: EXIT~n"),
			exit(self(), normal)
	end,
	telnet().

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
	connect(gen_tcp:connect(Host, Port, [])).

connect({ok, Socket})->
	Socket;
connect({error, Reason}) ->
	error.
	

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



