-module(earl).
-export([main/0, connect/0, buffer/0]).

main() ->
	register(parserPid, spawn(irc, buffer, [])),
	register(connectPid, spawn(irc, connect, [])).
	
connect() ->
	{ok, Socket} = gen_tcp:connect("irc.cs.kent.ac.uk", 6667, []),
	receive_data(Socket).
	
receive_data(Socket) ->
	receive
		{tcp, Socket, ":irc.cs.ukc.ac.uk NOTICE AUTH :*** No Ident response\r\n"} ->
			ok = gen_tcp:send(Socket, "USER jfp6 jfp6 jfp6 jfp6\n\rNICK Earl\n\rJOIN #bottesting");
		{tcp, Socket, Bin} -> 
			io:format("~p~n",[Bin]),
			parserPid ! Bin;
		{tcp_closed, Socket} ->
			"Connection closed.",
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
