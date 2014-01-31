-module(earl).
-export([main/0, connect/0, buffer/0]).

main() ->
	register(bufferPid, spawn(earl, buffer, [])),
	register(connectPid, spawn(earl, connect, [])).

connect({ok, Socket}) ->
	receive_data(Socket);
connect({error, Reason}) ->
	io:format("ERROR - Could not connect: ~s~n", [Reason]).
connect() ->
	connect(gen_tcp:connect("localhost", 6667, [], 1000)).
	
receive_data(Socket) ->
	receive
	    {tcp, Socket, ":irc.cs.ukc.ac.uk NOTICE AUTH :*** No Ident response\r\n"} ->
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

buffer() ->
    buffer([]).
buffer(Buffer)->
    receive
	Tmp1 -> Tmp1
    end,
    
    Tmp2 = Buffer ++ [Tmp1],
    io:format("BUGGER: ~p~n", [Tmp2]),
    buffer(Tmp2).


% For directed messages
send(Command, Target, Message, Socket) ->
    ok = gen_tcp:send(Socket, Command ++ " " ++ Target ++ " " ++ Message ++ "~n").

% For non-directed server comands, eg NICK and USER
send(Command, Message, Socket) ->
    ok = gen_tcp:send(Socket, Command ++ " " ++ Message ++ "~n").
