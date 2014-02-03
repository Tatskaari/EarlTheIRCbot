-module(telnet).
-export([telnet/1]).

telnet(SendPid) ->
	receive
		[From,_,_,Target,"#telnet " ++ K] ->
			%parser(From, Target, K, SendPid);
			SendPid ! {prvmsg, {From, Target, K}};
		die ->
			io:format("telnetPid :: EXIT~n")
	end,
	telnet(SendPid).

% will eventually parse the message and pass it to the right thread to do things 
parser(From, Target, K, SendPid) ->
	% register()
	SendPid ! {prvmsg, {From, Target, K}}.

% does things to message. Not srue what yet
cleanMessage(Message) ->
	Message. 

% connects to the server 
connect(HostName, Port) ->
	connect(gen_tcp:connect(HostName, Port, [], 1000)).
connect({ok, Socket}) ->
	Socket;
connect({error, Reason})->
	error.

% Receives data from the server and passes it to the sender	
receive_data(From, Target, SendPid, {server, {HostName, Port}}) ->
	Socket = connect(HostName, Port),
	if
		Socket == error ->
			false;
		true ->
			receive_data(From, Target, Socket, SendPid)
	end;
receive_data(From, Target, Socket, SendPid) ->
	receive
		die ->
			exit(self(), normal);
	    {tcp, Socket, Bin} ->
	    	Message = From ++ ": " ++ Bin, 
			SendPid ! {prvmsg, {From, Target, cleanMessage(Message)}};
	    {tcp_closed, Socket} ->
			io:format("Connection closed.~n")
	end,
    receive_data(From, Target, Socket, SendPid).
	


