-module(reminder).
-export([reminder/0, parse/1]).

%Contains the record definitions
-include("ircParser.hrl").

reminder() ->
	receive
		#privmsg{target=Target, from=From, message="#reminder " ++ Rest} ->
			{Time, Reminder} = parse(Rest),
			if
				Time == error ->
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
	{TimeString, Reminder} = {string:substr(Rest, 1, P-1), string:substr(Rest, P+1)},
	Time = parse(TimeString, time),
	{Time, Reminder}.
parse(Time, time) ->
	case string:tokens(Time, ":") of
		[Hours, Mins, Secs] ->
			Hour = stringToInt(Hours),
			Min = stringToInt(Mins),
			Sec = stringToInt(Secs),
			timeToSec({Hour, Min, Sec});
		[Mins, Secs] ->
			Hour = 0,
			Min = stringToInt(Mins),
			Sec = stringToInt(Secs),
			timeToSec({Hour, Min, Sec});
		[Secs] ->
			Hour = 0,
			Min = 0,
			Sec = stringToInt(Secs),
			timeToSec({Hour, Min, Sec});
		_ ->
			0
	end.
timeToSec({Hour, Min, Sec}) ->
	if 
		(Hour =/= error) and (Min =/= error) and (Sec =/= error) ->
			Hour * 60 * 60 + Min * 60 + Sec;
		true ->
			error
	end.


% converts a list of ints into the integer they represent 
stringToInt(Str) ->
	case string:to_integer(Str) of
        {error, _} -> 
        	error;
        {F,_Rest} -> 
        	if
        		F < 0 ->
        			error;
        		true ->
        			F
        	end
    end.

echoIn(Time, Message, {To, Target})->
	timer:sleep(Time * 1000),
	sendPid ! #privmsg{from=To, target=Target, message=To ++ ": " ++ Message}.
	