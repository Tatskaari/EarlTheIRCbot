-module(telnet).
-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2]).
-export([handle_info/2, code_change/3]).
-export([startSession/3, pidList/0]).

%Contains the record definitions
-include("ircParser.hrl").

% init the module then call the receive loop
init(_Args) ->
	Pid = spawn(telnet, pidList,[]),
	{ok, Pid}.

handle_event(#privmsg{target="#" ++ Target, from=From, message="#telnet" ++ _}, Pid) ->
	sendPid ! #privmsg{target=("#"++Target), message=(From ++ ": Please use private chat for telnet.")},
	{ok, Pid};

handle_event(#privmsg{from=From, message="#telnet connect " ++ K}, Pid) ->
	case string:tokens(K, " ") of
		[Host, Port] ->
			Pid ! {add, spawn(telnet, startSession,[Host, stringToInt(Port), From])};
		_ ->
			noMatch
	end,
	{ok, Pid};

handle_event(#privmsg{from=From, message="#telnet disconnect"}, Pid) ->
	Pid ! {disconnect, From},
	{ok, Pid};

handle_event(#privmsg{from=From, message="#telnet " ++ K}, Pid) ->
	Message = re:replace(K,"\\\\r\\\\n", "\r\n",[{return,list}]),
	Pid ! {sendMessage, Message, From},
	{ok, Pid};

handle_event(_Event, Pid) ->
	{ok, Pid}.

terminate(_Args, _State) ->
    ok.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% converts a list of ints into the integer they represent 
stringToInt(Str) ->
	case string:to_integer(Str) of
        {error, _} -> -1;
        {F,_Rest} -> F
    end.

% splits on new lines and echos the responce
echoResponce(A, From) ->
	Terms = string:tokens(A, "\r\n"),
	echoResponce(Terms, From, loop).
echoResponce([], _,loop) ->
	done;
echoResponce([Head|Tail], From, loop) ->
	sendPid ! #privmsg{target=From, message=Head},
	timer:sleep(1000),
	echoResponce(Tail, From, loop).

% starts a new session 
startSession(Host, Port, From) ->
	case connect(Host, Port) of
		{error, Reason} ->
			sendPid ! #privmsg{target=From, message="Failed to connect: " ++ Reason};
		Socket ->
			receive_data(Socket, From)
	end.

% connects to the server
connect(Host, Port) ->
	connect(gen_tcp:connect(Host, Port, [], 1000)).
connect({ok, Socket})->
	Socket;
connect({error, Reason}) ->
	{error, Reason}.

% each person should get their own socket and pid to receive data on. 
receive_data(Socket, From) ->
	receive
		{tcp, Socket, Bin} ->
			echoResponce(Bin, From);
		{tcp_closed, Socket} ->
			sendPid !  #privmsg{target=From, message="Connection closed by foreign host."},
			self() ! die;
		{command, Message, From} ->
			io:format("TELNET SEND:: ~s: ~s~n", [From, Message]),
			gen_tcp:send(Socket, Message);
		{disconnect, From} ->
			self() ! die;
		die ->
			sendPid ! #privmsg{target=From, message="Exiting session."},
			pidListPid ! {remove, self()},
			gen_tcp:close(Socket),
			exit(self(), normal)
	end,
	receive_data(Socket, From).

% Keeps a list of pids to message
pidList() ->
	pidList([]).
pidList(PidList) ->
	receive
		{add, Pid} ->
			pidList(PidList ++ [Pid]);
		{remove, Pid} ->
			pidList(PidList -- [Pid]);
		{getList, _Pid} ->
			io:format("~p~n", PidList);
		{disconnect, From} ->
			lists:foreach(fun(Pid) -> Pid ! {disconnect , From} end, PidList);
		{sendMessage, Message, From} ->
			lists:foreach(fun(Pid) -> Pid ! {command, Message, From} end, PidList);
		die ->
			lists:foreach(fun(Pid) -> Pid ! die end, PidList),
			exit(self(), normal)
	end,
	pidList(PidList).

% helper functoin to send each pid a message
sendPidsMessage([], _, _) ->
	ok;
sendPidsMessage([Head|Tail], Message, From) ->
	Head ! {command, Message, From},
	sendPidsMessage(Tail, Message, From).
