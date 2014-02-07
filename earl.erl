-module(earl).
-export([main/0, buffer/0,buffer/1, send/1, getLine/1, setting_server/0]).
-import(messageRouter, [route/0]).
-include("ircParser.hrl").
-include("earl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("earl_test.erl").


% Spawns the buffer and the connections processes
main() ->
	% Spawn the processes for connecting and building commmands
	register(bufferPid, spawn(earl, buffer, [])),        
	register(connectPid, spawn(earlConnection, connect, [?HOSTNAME, ?PORT, self()])),
	register(routerPid, spawn(messageRouter, route, [])),
	register(mainPid, self()),
	register(settings, spawn(fun() -> setting_server() end)),
	register(channel_info, spawn(fun() -> setting_server() end)),

	% Start the plugins
	start(),

	receive
		connected -> true
	end,

	sendPid ! #user{user=?USER},
	sendPid ! #nick{nick=?NICK},
	
	% Wait until a process wants to kill the program and then tell all processes to an hero 
	receive
		die ->
			bufferPid ! die,
			routerPid ! die,
			connectPid ! die,
			settings ! die,
			io:format("mainPid :: EXIT~n"),
			exit(self(), normal)
	end.

start() ->
	% Set up admin list
	settings ! #setVal{name=admins, value=["graymalkin", "Tatskaari", "Mex", "xand", "Tim"]},

	% Send module registrations
	routerPid ! #registerPlugin{name="earlAdminPlugin"},
	routerPid ! #registerPlugin{name="optimusPrime"},
	routerPid ! #registerPlugin{name="telnet"},
	routerPid ! #registerPlugin{name="ircTime"}.

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
			routerPid ! A,
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
				_ when From == undefined ->
					M = "PRIVMSG " ++ Target ++ " :" ++ Message ++ "\r\n",
					io:format("SENT: ~s", [M]),
					ok = gen_tcp:send(Socket, M);

				% When a message was sent to a channel, send response to channel
				"#" ++ _ ->
					M = "PRIVMSG " ++ Target ++ " :" ++ Message ++ "\r\n",
					io:format("SENT: ~s", [M]),
					ok = gen_tcp:send(Socket, M);

				% Otherwise send to the originator
				_ ->
					M = "PRIVMSG " ++ From ++ " :" ++ Message ++ "\r\n",
					io:format("SENT: ~s", [M]),
					ok = gen_tcp:send(Socket, M)
			end;

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
					Chan ! #retVal{name=Name, value=dict:fetch(Name, Dict)};
				false ->
					Chan ! #noVal{name=Name}
			end;
		die ->
			io:format("settings :: EXIT~n"),
			exit(self(), normal)	
	end,
	setting_server(Dict).
