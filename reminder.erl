-module(reminder).
-export([reminder/0, eggtimerParser/1, stringToInt/1]).

% test test test
-include_lib("eunit/include/eunit.hrl").
-include("reminder_test.erl").

%Contains the record definitions
-include("ircParser.hrl").

reminder() ->
	receive
		#privmsg{target=Target, from=From, message="#reminder " ++ Rest} ->
			case dateParser(Rest) of
				{GregorianTime, Reminder} ->
					echoAt(GregorianTime, Reminder, {From, Target});
				_ ->
					sendPid ! #privmsg{target=Target, from=From, message=From ++ ": Input error"}
			end;
		#privmsg{target=Target, from=From, message="#eggtimer " ++ Rest} ->
			case eggtimerParser(Rest) of
				error ->
					sendPid ! #privmsg{target=Target, from=From, message=From ++ ": Input error"};
				Time ->
					echoIn(Time, {From, Target})
			end;
		die ->
			io:format("reminder :: EXIT")
	end,
	reminder().

echoIn(Time, {To, Target}) ->
	timer:sleep(Time * 1000),
	sendPid ! #privmsg{from=To, target=Target, message=To ++ ": Your timer is up!"}.
echoAt(Time, Message, {To, Target}) ->
	CurrentTime = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	SleepTime = Time - CurrentTime,
	if
		SleepTime < 0 ->
			sendPid ! #privmsg{from=To, target=Target, message=To ++ ": Error, time is in the past"};
		true ->
			timer:sleep(SleepTime * 1000),
			sendPid ! #privmsg{from=To, target=Target, message=To ++ ": Reminder '" ++ Message ++ "'"}
	end.

eggtimerParser(String) ->
	TimeTuple = case string:tokens(String, ":") of
		[Hour, Min, Sec] ->
			{stringToInt(Hour), stringToInt(Min), stringToInt(Sec)};
		[Min, Sec] ->
			{0, stringToInt(Min), stringToInt(Sec)};
		[Min] ->
			{0, stringToInt(Min), 0};
		_ ->
			error
	end,
	case TimeTuple of
		{error, _, _} ->
			error;
		{_, error, _} ->
			error;
		{_, _, error} ->
			error;
		{Hours, Mins, Secs} ->
			Hours * 60 * 60 + Mins * 60 + Secs;
		_ ->
			error
	end.

% converts an inputted string into a date time stamp and the reminder
dateParser(String) ->
	{Time, Rest} = token(String, " "),
	{Date, Reminder} = token(Rest, " "),
	DateTuple = dateStringToTuple(Date),
	TimeTuple = timeStringToTuple(Time),
	case {validDate(DateTuple), validTime(TimeTuple)} of
		{true, true} ->
			{calendar:datetime_to_gregorian_seconds({DateTuple, TimeTuple}), Reminder};
		{false, true} ->
			{CurrentDate, _} = erlang:localtime(),
			{calendar:datetime_to_gregorian_seconds({CurrentDate, TimeTuple}), Rest};
		_ ->
			error
			
	end.
		

timeStringToTuple(TimeString) ->
	case string:tokens(TimeString, ":") of
		[Hour, Min, Sec] ->
			{stringToInt(Hour), stringToInt(Min), stringToInt(Sec)};
		[Hour, Min] ->
			{stringToInt(Hour), stringToInt(Min), 0};
		[Hour] ->
			{stringToInt(Hour), 0, 0};
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
	if
		P<1 ->
			{"", String};
		true ->
			{string:substr(String, 1, P-1), string:substr(String, P+1)}
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
        		_Rest =/= "" ->
        			error;
        		true ->
        			F
        	end
    end.

% input validation helper methods
validTime({Hour, Min, Sec}) ->
	if
		(Hour > 24) or (Min > 59) or (Sec > 59)->
			false;
		true ->
			true
	end;
validTime(_) ->
	false.


validDate({error, _, _})->
	false;
validDate({_, error, _})->
	false;
validDate({_, _, error})->
	false;	
validDate({Year, Month, Day}) ->
	calendar:valid_date({Year, Month, Day});
validDate(_) ->
	false.