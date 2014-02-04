-module(earlConnection).
-export([connect/2]).

% Opens a connectoin to the server
connect({ok, Socket}) ->
	register(sendPid, spawn(earl, send, [Socket])),
	receive_data(Socket);
connect({error, Reason}) ->
	io:format("ERROR - Could not connect: ~s~n", [Reason]).
connect(Hostname, Port) ->
	io:format("INFO - Connecting to ~s:~p ~n", [Hostname, Port]),
	connect(gen_tcp:connect(Hostname, Port, [], 1000)).


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
