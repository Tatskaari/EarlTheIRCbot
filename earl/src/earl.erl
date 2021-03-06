-module(earl).
-export([buffer/0, buffer/1, send/1, getLine/1]).
-import(messageRouter, [parse/0]).
-include("ircParser.hrl").
-include("earl.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(application).

-export([start/2, stop/1]).

stop(_State) ->
    bufferPid ! die,
    parserPid ! die,
    connectPid ! die,
    settingsServer:stop(settings),
    settingsServer:stop(channel_info), % incomplete, this Server has sub servers...
    io:format("mainPid :: EXIT~n"),
    exit(self(), normal),
    ok.

% Spawns the buffer and the connections processes
start(_Type, _Args) ->
	% Spawn the processes for connecting and building commmands
	register(bufferPid, spawn(earl, buffer, [])),        
	register(connectPid, spawn(earlConnection, connect, [?HOSTNAME, ?PORT])),
	register(parserPid, spawn(messageRouter, parse, [])),
	register(mainPid, self()),
	{ok, SettingsPid} = settingsServer:start_link("settings.db"),
	{ok, ChanInfoPid} = settingsServer:start_link(),
	register(settings, SettingsPid),
	register(channel_info, ChanInfoPid),


	gen_event:start_link({local, irc_messages}),
	% Start the plugins
	setup(),

	receive
		connected -> true
	end,

	% Wait until a process wants to kill the program and then tell all processes to an hero 
        %% TODO: Move the close code into stop/1
    K = earl_sup:start_link(),
    io:format("~p~n", [K]),
    K.

setup() ->
	% Set up admin list
	settingsServer:setValue(settings, admins, ["graymalkin", "Tatskaari", "Mex", "xand", "Tim"]),

	% Send module registrations
	lists:foreach(fun(Plugin) -> parserPid ! #registerPlugin{name=Plugin} end, ?PLUGINS).

getLine(A) ->
	Index = string:str(A, "\n"),
	case Index of
		0 -> {false, A};
		_ -> {true, string:substr(A, 1, Index-1), string:substr(A,Index+1)}
	end.



% Builds the messages sent by the server and prints them out
buffer() ->
	buffer("").
buffer(Buffer) ->
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
