-module(ircParser).
-export([parse/1]).

parse(SendPid) ->
    receive
		["PING :" ++ T] ->
		    SendPid ! {command, {"PONG", T}};

    end,
    parse(SendPid).

