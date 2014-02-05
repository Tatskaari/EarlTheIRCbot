-module(earlConnection).
-export([connect/3]).

% Opens a connectoin to the server
connect({ok, Socket}, ReadyChan) ->
	register(sendPid, spawn(earl, send, [Socket])),
	ReadyChan ! connected,
	receive_data(Socket);
connect({error, Reason},_) ->
	io:format("ERROR - Could not connect: ~s~n", [Reason]).
connect(Hostname, Port, ReadyChan) ->
	io:format("INFO - Connecting to ~s:~p ~n", [Hostname, Port]),
	connect(gen_tcp:connect(Hostname, Port, [], 1000), ReadyChan).


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
