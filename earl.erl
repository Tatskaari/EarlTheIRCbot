-module(earl).
-export([main/0, connect/0, buffer/0, send/1]).
-import(ircParser, [parse/1]).

% Spawns the buffer and the connections processes
main() ->
	register(bufferPid, spawn(earl, buffer, [])),        
	register(connectPid, spawn(earl, connect, [])).

% Opens a connectoin to the server
connect({ok, Socket}) ->
	register(sendPid, SendPid = spawn(earl, send, [Socket])),
    register(parserPid, spawn(ircParser, parse, [SendPid])),
	receive_data(Socket);
connect({error, Reason}) ->
	io:format("ERROR - Could not connect: ~s~n", [Reason]).
connect() ->
	connect(gen_tcp:connect("irc.cs.kent.ac.uk", 6667, [], 1000)).

% Receives data from the server and passes it to buffer
receive_data(Socket) ->
	receive
	    {tcp, Socket, ":irc.cs.ukc.ac.uk NOTICE AUTH :*** Got Ident response\r\n"} ->
	    	io:format("Loggin' in YOLO!~n", []),
	    	sendPid ! {command, {"USER", "Sir_Earl Sir_Earl Sir_Earl Sir_Earl"}},
	    	sendPid ! {command, {"NICK", "Earl2"}};
	    {tcp, Socket, Bin} -> 
			bufferPid ! Bin;
	    {tcp_closed, Socket} ->
			io:format("Connection closed.~n",[]),
			exit(self(), normal);
		{send, A} ->
			gen_tcp:send(Socket, A)
	end,
	receive_data(Socket).

% Builds the messages sent by the server and prints them out
buffer() ->
	buffer([]).
buffer(Buffer)->
	receive
		Bin -> 
			Cond = string:str(Bin, "\n") == 0,
			if
				Cond ->
					buffer(Buffer ++ [Bin]);
				true ->
					io:format("~s~n", Buffer ++ [Bin]),
				    parserPid ! Buffer ++ [Bin],
					buffer([])
			end
	end.

% new shiny send message box that sends commands to the server
send(Socket) ->
	receive
		{command, {Command, Target, Message}} ->
			M = Command ++ " " ++ Target ++ " " ++ Message ++ "\n\r",
		    io:format("SENT :: ~s", [M]),
			ok = gen_tcp:send(Socket, M);
		{command, {Command, Message}} ->
			M = Command ++ " " ++ Message ++ "\n\r",
		    io:format("SENT :: ~s", [M]),
			ok = gen_tcp:send(Socket, M)
	end,
	send(Socket).

%parse() ->                             
%    receive
%        ["PING :" ++ T] ->
%            io:format("PING ~s~n", [T]),
%	    sendPid ! { "PONG", T };
%	N ->
%	    io:format("~s~n", [N])
%    end,
%    parse().
%
