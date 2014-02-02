-module(earl).
-export([main/0, connect/0, buffer/0, send/1, getLine/1]).
-import(ircParser, [start/1]).
-include_lib("eunit/include/eunit.hrl").

-define(HOSTNAME, "irc.cs.kent.ac.uk").
-define(PORT, 6667).



% Spawns the buffer and the connections processes
main() ->
	% Spawn the processes for connecting and building commmands
	register(bufferPid, spawn(earl, buffer, [])),        
	register(connectPid, spawn(earl, connect, [])),
	register(mainPid, self()),

	% Wait until a process wants to kill the program and then tell all processes to an hero 
	receive
		die ->
			bufferPid ! die,
			sendPid ! die,
			parserPid ! die,
			connectPid ! die,
			exit(self(), normal)
	end.

% Opens a connectoin to the server
connect({ok, Socket}) ->
	register(sendPid, SendPid = spawn(earl, send, [Socket])),
	register(parserPid, spawn(ircParser, start, [SendPid])),
	receive_data(Socket);
connect({error, Reason}) ->
	io:format("ERROR - Could not connect: ~s~n", [Reason]).
connect() ->
	io:format("Connecting to ~s~n", [?HOSTNAME]),
	connect(gen_tcp:connect(?HOSTNAME, ?PORT, [], 1000)).

% Receives data from the server and passes it to buffer
receive_data(Socket) ->
	receive
		die ->
			io:format("connectPid :: EXIT~n"),
			exit(self(), normal);
	    {tcp, Socket, Bin} -> 
			bufferPid ! Bin;
	    {tcp_closed, Socket} ->
			io:format("Connection closed.~n",[]),
			mainPid ! die
	end,
    receive_data(Socket).

getLine_test() ->
	?assert(getLine("abc") == {false, "abc"}),
	?assert(getLine("") == {false, ""}),
	{A,B,C} = getLine("abc\ndef"),
	?assert(A == true),
	?assert(B == "abc"),
	?assert(C == "def"),
	{D,E,F} = getLine("abc\n"),
	?assert(D == true),
	?assert(E == "abc"),
	?assert(F == "").

getLine(A) ->
	Index = string:str(A, "\n"),
	case Index of
		0 -> {false, A};
		_ -> {true, string:substr(A, 1, Index-1), string:substr(A,Index+1)}
	end.



% Builds the messages sent by the server and prints them out
buffer() ->
	buffer("").
buffer(Buffer)->
	case getLine(Buffer) of
		{false, _ } ->
			receive
				die ->
					io:format("bufferPid :: EXIT~n"),
					exit(self(), normal);
				Bin -> 
					buffer(Buffer ++ Bin)
			end;
		{true, A, B} ->
			io:format("RECEIVED :: ~s~n", [A]),
			parserPid ! A,
			buffer(B)
	end.

% new shiny send message box that sends commands to the server
send(Socket) ->
	receive
		die ->
			io:format("sendPid :: EXIT~n"),
			exit(self(), normal);
		{command, {Command, Target, Message}} ->
			M = Command ++ " " ++ Target ++ " :" ++ Message ++ "\n\r",
		    io:format("SENT :: ~s", [M]),
			ok = gen_tcp:send(Socket, M);
		{command, {Command, Message}} ->
			M = Command ++ " " ++ Message ++ "\n\r",
		    io:format("SENT :: ~s", [M]),
			ok = gen_tcp:send(Socket, M)
	end,
	send(Socket).
