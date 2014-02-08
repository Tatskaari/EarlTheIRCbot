-module(reminder).
-export([reminder/0, parse/1]).

%Contains the record definitions
-include("ircParser.hrl").

reminder() ->
	receive
		#privmsg{target=Target, from=From, message="#reminder " ++ Rest} ->
			{Time, Reminder} = parse(Rest),
			io:format("~p~n",[Time]),
			if
				Time < 0 ->
					Message = From ++ ": Input error.",
					sendPid ! #privmsg{from=From, target=Target, message=Message},
					reminder();
				true ->
					spawn(fun() -> echoIn(Time, Reminder, {From, Target}) end)
			end;
		die ->
			io:format("reminder :: EXIT")
	end,
	reminder().

% tokenises the params
parse(Rest) ->
	P = string:str(Rest, " "),
	{Time, Reminder} = {string:substr(Rest, 1, P-1), string:substr(Rest, P+1)},
	{stringToInt(Time), Reminder}.

% converts a list of ints into the integer they represent 
stringToInt(Str) ->
	case string:to_integer(Str) of
        {error, _} -> -1;
        {F,_Rest} -> F
    end.

echoIn(Time, Message, {To, Target})->
	timer:sleep(Time * 1000),
	sendPid ! #privmsg{from=To, target=Target, message=Message}.
	