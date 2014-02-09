-module(reminder).
-export([reminder/0, dateParser/1]).

%Contains the record definitions
-include("ircParser.hrl").

reminder() ->
	receive
		#privmsg{target=Target, from=From, message="#reminder " ++ Rest} ->
			case dateParser(Rest) of
				{GregorianTime, Reminder} ->
					echoAt(GregorianTime, Reminder, {From, Target});
				_ ->
					#privmsg{target=Target, from=From, message=From ++ ": Input error"}
			end;
		die ->
			io:format("reminder :: EXIT")
	end,
	reminder().

echoIn(Time, Message, {To, Target}) ->
	timer:sleep(Time * 1000),
	sendPid ! #privmsg{from=To, target=Target, message=To ++ ": " ++ Message}.
echoAt(Time, Message, {To, Target}) ->
	CurrentTime = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	SleepTime = Time - CurrentTime,
	if
		SleepTime < 0 ->
			sendPid ! #privmsg{from=To, target=Target, message=To ++ ": Error, time is in the past"};
		true ->
			timer:sleep(SleepTime * 1000),
			sendPid ! #privmsg{from=To, target=Target, message=To ++ ": Reminder, '" ++ Message ++ "'"}
	end.

% converts an inputted string into a date time stamp and the reminder
dateParser(String) ->
	{Time, Rest} = token(String, " "),
	{Date, Reminder} = token(Rest, " "),
	DateTuple = dateStringToTuple(Date),
	TimeTuple = timeStringToTuple(Time),
	case (validDate(DateTuple) and validTime(TimeTuple)) of
		true ->
			{calendar:datetime_to_gregorian_seconds({DateTuple, TimeTuple}), Reminder};
		_ ->
			error
	end.
		

timeStringToTuple(TimeString) ->
	case string:tokens(TimeString, ":") of
		[Hour, Min, Sec] ->
			{stringToInt(Hour), stringToInt(Min), stringToInt(Sec)};
		[Min, Sec] ->
			{0, stringToInt(Min), stringToInt(Sec)};
		[Sec] ->
			{0, 0, stringToInt(Sec)};
		_ -> 
			error
	end.

dateStringToTuple(DateSting) ->
	case string:tokens(DateSting, "/") of
		[Day, Month, Year] ->
			{stringToInt(Year), stringToInt(Month), stringToInt(Day)};
		_ -> 
			error
	end.

% utils

% gets the first token in String splitting on Splitter.
token(String, Splitter) ->
	P = string:str(String, Splitter),
	{Token, Rest} = {string:substr(String, 1, P-1), string:substr(String, P+1)}.

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

validTime({Hour, Min, Sec}) ->
	if
		(Hour > 24) or (Min > 59) or (Sec > 59)->
			false;
		true ->
			true
	end;
validTime(_) ->
	false.

validDate({Year, Month, Day}) ->
	calendar:valid_date({Year, Month, Day});
validDate(_) ->
	false.