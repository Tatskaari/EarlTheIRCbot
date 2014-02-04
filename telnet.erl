-module(telnet).
-export([telnet/0, startSession/3]).

%Contains the record definitions
-include("ircParser.hrl").

telnet() ->
	receive
		#privmsg{target="#" ++ Target, from=From} ->
			sendPid ! {prvmsg, {From, "#" ++ Target, From ++ ": Please use private chat for telnet."}};
		#privmsg{target=Target, from=From, message="#telnet " ++ K} ->
			case string:tokens(K, " ") of
				[Host, Port] ->
					spawn(telnet, startSession,[Host, stringToInt(Port), From]);
				_ ->
					noMatch
			end;
		die ->
			io:format("telnetPid :: EXIT~n"),
			exit(self(), normal)
	end,
	telnet().
% starts a new session 
startSession(Host, Port, From) ->
	case connect(Host, Port) of
		{error, Reason} ->
			sendPid ! {command, {"PRIVMSG", From, "Failed to connect: " ++ Reason}};
		Socket ->
			receive_data(Socket, From);
		_ ->
			dafaqisthis
	end.
receive_data(Socket, From) ->
	receive
		{tcp, Socket, Bin} ->
			sendPid ! {command, {"PRIVMSG", From, Bin}};
		{tcp_close, Socket} ->
			sendPid ! {prvmsg, {From, From, "Connection closed by forein host."}},
			exit(self(), normal);
		die ->
			exit(self(), normal)
	end,
	receive_data(Socket, From).
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
	{error, Reason}.
	



