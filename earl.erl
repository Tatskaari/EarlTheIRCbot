-module(earl).
-export([main/0, connect/0, buffer/0]).

% Spawns the buffer and the connections processes
main() ->
	register(bufferPid, spawn(earl, buffer, [])),
	register(connectPid, spawn(earl, connect, [])).

% Opens a connectoin to the server
connect({ok, Socket}) ->
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
			send("USER", "Sir_Earl Sir_Earl Sir_Earl Sir_Earl", Socket),
			send("NICK", "Earl", Socket);
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
					io:format("~p~n", Buffer ++ [Bin]),
					buffer([])
			end
	end.

% For directed messages
send(Command, Target, Message, Socket) ->
    ok = gen_tcp:send(Socket, Command ++ " " ++ Target ++ " " ++ Message ++ "\n\r").

% For non-directed server comands, eg NICK and USER
send(Command, Message, Socket) ->  
    ok = gen_tcp:send(Socket, Command ++ " " ++ Message ++ "\n\r").