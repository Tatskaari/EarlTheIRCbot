-module(telnet).
-export([telnet/0, startSession/3, pidList/0]).

%Contains the record definitions
-include("ircParser.hrl").

telnet() ->
	register(pidListPid, spawn(telnet, pidList,[])),
	loop().
loop() ->
	receive
		#privmsg{target="#" ++ Target, from=From, message="#telnet" ++ _} ->
			sendPid ! {prvmsg, {From, "#" ++ Target, From ++ ": Please use private chat for telnet."}};
		#privmsg{from=From, message="#telnet connect " ++ K} ->
			case string:tokens(K, " ") of
				[Host, Port] ->
					pidListPid ! {add, spawn(telnet, startSession,[Host, stringToInt(Port), From])};
				_ ->
					noMatch
			end;
		#privmsg{from=From, message="#telnet " ++ K} ->
			Message = re:replace(K,"\\\\r\\\\n", "\r\n",[{return,list}]),
			pidListPid ! {sendMessage, Message, From};
		die ->
			io:format("telnetPid :: EXIT~n"),
			exit(self(), normal)
	end,
	loop().

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
	timer:sleep(1000),
	echoResponce(Tail, From, loop).

% starts a new session 
startSession(Host, Port, From) ->
	case connect(Host, Port) of
		{error, Reason} ->
			sendPid ! {command, {"PRIVMSG", From, "Failed to connect: " ++ Reason}};
		Socket ->
			receive_data(Socket, From)
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
		{tcp_closed, Socket} ->
			sendPid ! {prvmsg, {From, From, "Connection closed by forein host."}},
			exit(self(), normal);
		{command, Message, From} ->
			gen_tcp:send(Socket, Message);
		die ->
			exit(self(), normal);
		Wut ->
			io:format("~p~n", [Wut])
	end,
	receive_data(Socket, From).

% Keeps a list of pids to message
pidList() ->
	pidList([]).
pidList(PidList) ->
	receive
		{add, Pid} ->
			pidList(PidList ++ [Pid]);
		{remove, Pid} ->
			pidList(PidList -- [Pid]);
		{getList, Pid} ->
			io:format("~p~n", PidList);
		{sendMessage, Message, From} ->
			sendPidsMessage(PidList, Message, From);
		die ->
			exit(self(), normal)
	end,
	pidList(PidList).
	
% helper functoin to send each pid a message
sendPidsMessage([], _, _) ->
	ok;
sendPidsMessage([Head|Tail], Message, From) ->
	io:format("TELNET SEND :: ~p~n", [Message]),
	Head ! {command, Message, From},
	sendPidsMessage(Tail, Message, From).