-module(earl).
-export([main/0, buffer/0,buffer/1, send/1, getLine/1]).
-import(ircParser, [parse/0]).
-import(optimusPrime, [optimusPrime/0]).
-import(time, [timer/0]).
-import(telnet, [telnet/0]).
-include_lib("eunit/include/eunit.hrl").
-include("ircParser.hrl").
-include("earl.hrl").
-include_lib("earl_test.erl").


% Spawns the buffer and the connections processes
main() ->
	% Spawn the processes for connecting and building commmands
	register(bufferPid, spawn(earl, buffer, [])),        
	register(connectPid, spawn(earlConnection, connect, [?HOSTNAME, ?PORT])),
	register(parserPid, spawn(ircParser, parse, [])),
	register(mainPid, self()),
	register(settings, spawn(fun() -> setting_server() end)),

	% Start the plugins
	start(),

	% Wait until a process wants to kill the program and then tell all processes to an hero 
	receive
		die ->
			bufferPid ! die,
			sendPid ! die,
			parserPid ! die,
			connectPid ! die,
			settings ! die,
			io:format("mainPid :: EXIT~n")
			exit(self(), normal)
	end.

start() ->
	parserPid ! #registerPlugin{chan=spawn(earlAdminPlugin, start, [sendPid])},
	parserPid ! #registerPlugin{chan=(spawn(optimusPrime, optimusPrime, []))},
	parserPid ! #registerPlugin{chan=(spawn(telnet, telnet, []))},
	parserPid ! #registerPlugin{chan=(spawn(ircTime, ircTime, []))}.

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

		#privmsg{from=From, target=Target, message=Message} ->
			case Target of
				% When a message was sent to a channel, send response to channel
				"#" ++ _ ->
					M = Command ++ " " ++ Target ++ " :" ++ Message ++ "\r\n",
					io:format("SENT: ~s", [M]),
					ok = gen_tcp:send(Socket, M);

				% Otherwise send to the originator
				_ ->
					M = Command ++ " " ++ From ++ " :" ++ Message ++ "\r\n",
					io:format("SENT: ~s", [M]),
					ok = gen_tcp:send(Socket, M)
			end;
		#privmsg{target=Target, message=Message} ->
			M = Command ++ " " ++ Target ++ " :" ++ Message ++ "\r\n",
			io:format("SENT: ~s", [M]),
			ok = gen_tcp:send(Socket, M);

		#command{command=Command, data=Data} ->
			M = Command ++ " " ++ Data ++ "\r\n",
		    io:format("SENT: ~s", [M]),
			ok = gen_tcp:send(Socket, M);

		#ping{nonce=N} ->
			M = "PING :" ++ N ++ "\r\n",
			io:format("SENT: ~s", [M]),
			ok = gen_tcp:send(Socket, M);

		#pong{nonce=N} ->
			M = "PONG " ++ N ++ "\r\n",
			io:format("SENT: ~s", [M]),
			ok = gen_tcp:send(Socket, M);

		#user{user=U} ->
			M = "USER " ++ U ++ "\r\n",
			io:format("SENT: ~s", [M]),
			ok = gen_tcp:send(Socket, M);

		#nick{nick=N} ->
			M = "NICK " ++ N ++ "\r\n",
			io:format("SENT: ~s", [M]),
			ok = gen_tcp:send(Socket, M);

		#join{channel=C} ->
			M = "JOIN " ++ C ++ "\r\n",
			io:format("SENT: ~s", [M]),
			ok = gen_tcp:send(Socket, M);

		#part{channel=C} ->
			M = "PART " ++ C ++ "\r\n",
			io:format("SENT: ~s", [M]),
			ok = gen_tcp:send(Socket, M);

		#quit{reason=R} ->
			M = "QUIT :" ++ R ++ "\r\n",
			io:format("SENT: ~s", [M]),
			ok = gen_tcp:send(Socket, M);

		#raw{data=Data} ->
			io:format("SENT :: ~s~n", [Data]),
			ok = gen_tcp:send(Socket, Data)
	end,
	send(Socket).

setting_server() -> setting_server(dict:new()).

setting_server(Dict) ->
	receive
		#setVal{name=Name, value=Value} -> 
			setting_server(dict:store(Name, Value, Dict));
		#getVal{name=Name, return_chan=Chan} ->
			case dict:is_key(Name, Dict) of
				true ->
					Chan ! dict:fetch(Name, Dict);
				false ->
					Chan ! false
			end;
		die ->
			io:format("settings :: EXIT~n"),
			exit(self(), normal)	
	end,
	setting_server(Dict).
