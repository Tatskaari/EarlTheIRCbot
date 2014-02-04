-module(telnet).
-export([telnet/0, startSession/3]).

%Contains the record definitions
-include("ircParser.hrl").

telnet() ->
	receive
		#privmsg{target="#" ++ Target, from=From, message="#telnet" ++ _} ->
			sendPid ! {prvmsg, {From, "#" ++ Target, From ++ ": Please use private chat for telnet."}};
		#privmsg{target=Target, from=From, message="#telnet connect " ++ K} ->
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

% converts a list of ints into the integer they represent 
stringToInt(Str) ->
	case string:to_integer(Str) of
        {error, _} -> -1;
        {F,_Rest} -> F
    end.
% splits on new lines and echos the responce
echoResponce(A, From) ->
	Terms = string:tokens(A, "\r\n"),
	echoResponce(Terms, From, loop).
echoResponce([], _,loop) ->
	done;
echoResponce([Head|Tail], From, loop) ->
	sendPid ! {command, {"PRIVMSG", From, Head}},
	echoResponce(Tail, From, loop).

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

% connects to the server
connect(Host, Port) ->
	connect(gen_tcp:connect(Host, Port, [], 1000)).
connect({ok, Socket})->
	Socket;
connect({error, Reason}) ->
	{error, Reason}.

receive_data(Socket, From) ->
	receive
		{tcp, Socket, Bin} ->
			echoResponce(Bin, From);
		{tcp_close, Socket} ->
			sendPid ! {prvmsg, {From, From, "Connection closed by forein host."}},
			exit(self(), normal);
		die ->
			exit(self(), normal)
	end,
	receive_data(Socket, From).

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