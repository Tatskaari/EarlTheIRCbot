-module(ircParser).
-export([parse/1]).

parse(SendPid) ->
    receive
	["PING :" ++ T] ->
	    SendPid ! { "PONG", T }
    end,
    parse(SendPid).
