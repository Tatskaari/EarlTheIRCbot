-module(earl).
-export([main/0, buffer/0,buffer/1, send/1, getLine/1]).
-import(ircParser, [parse/0]).
-import(optimusPrime, [optimusPrime/0]).
-import(time, [timer/0]).
-import(telnet, [telnet/0]).
-include_lib("eunit/include/eunit.hrl").
-include("ircParser.hrl").

-define(HOSTNAME, "irc.cs.kent.ac.uk").
-define(PORT, 6667).



% Spawns the buffer and the connections processes
main() ->
	% Spawn the processes for connecting and building commmands
	register(bufferPid, spawn(earl, buffer, [])),        
	register(connectPid, spawn(earlConnection, connect, [?HOSTNAME, ?PORT])),
	register(parserPid, spawn(ircParser, parse, [])),
	register(mainPid, self()),

	% Start the plugins
	start(),

	% Wait until a process wants to kill the program and then tell all processes to an hero 
	receive
		die ->
			bufferPid ! die,
			sendPid ! die,
			parserPid ! die,
			connectPid ! die,
			exit(self(), normal)
	end.

start() ->
	parserPid ! #registerPlugin{chan=spawn(earlAdminPlugin, start, [sendPid])},
	parserPid ! #registerPlugin{chan=(spawn(optimusPrime, optimusPrime, []))},
	parserPid ! #registerPlugin{chan=(spawn(telnet, telnet, []))},
	parserPid ! #registerPlugin{chan=(spawn(timer, timer, []))}.

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
			parserPid ! A,
			?MODULE:buffer(B)
	end.



% new shiny send message box that sends commands to the server
send(Socket) ->
	receive
		die ->
			io:format("sendPid :: EXIT~n"),
			exit(self(), normal);
		{command, {Command, Target, Message}} ->
			M = Command ++ " " ++ Target ++ " :" ++ Message ++ "\r\n",
		    io:format("SENT :: ~s", [M]),
			ok = gen_tcp:send(Socket, M);
		{command, {Command, Message}} ->
			M = Command ++ " " ++ Message ++ "\r\n",
		    io:format("SENT :: ~s", [M]),
			ok = gen_tcp:send(Socket, M);
		{prvmsg, {From, Target, Message}} ->
			io:format("Got prvmsg~n"),
			case Target of
				"#" ++ _Channel ->
					sendPid ! {command, {"PRIVMSG", Target, Message}};
				_UserName ->
					sendPid ! {command, {"PRIVMSG", From, Message}}
			end;
		{raw, {Data}} ->
			io:format("SENT :: ~s~p", [Data]),
			ok = gen_tcp:send(Socket, Data)
	end,
	send(Socket).
