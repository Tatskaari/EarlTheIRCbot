-module(ning).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).
-export([handle_info/2, code_change/3]).

-include("ircParser.hrl").


init(_Args) ->
    {ok, []}.

handle_event(#privmsg{target=Target, from=From, message="#ning!" ++K}, State) ->
    Ning = get_ning(erlang:localtime()),
    case Ning of
	evening -> sendPid ! #privmsg{from=From, target=Target, message="Evening" ++ K ++ "!"};
	morning -> sendPid ! #privmsg{from=From, target=Target, message="Morning" ++ K ++ "!"};
	afternooning -> sendPid ! #privmsg{from=From, target=Target, message="Afternooning" ++ K ++ "!"}
    end,
    {ok, State};
handle_event(#privmsg{target=Target, from=From, message="#NING!" ++ K}, State) -> 
    Ning = get_ning(erlang:localtime()),
    case Ning of
	evening -> sendPid ! #privmsg{from=From, target=Target, message="EVENING" ++ K ++ "!"};
	morning -> sendPid ! #privmsg{from=From, target=Target, message="MORNING" ++ K ++ "!"};
	afternooning -> sendPid ! #privmsg{from=From, target=Target, message="AFTERNOONING" ++ K ++ "!"}
    end,
    {ok, State};
handle_event(#privmsg{target=Target, from=From, message="#ning" ++ K}, State) ->
    Ning = get_ning(erlang:localtime()),
    case Ning of
	evening -> sendPid ! #privmsg{from=From, target=Target, message="Evening" ++ K};
	morning -> sendPid ! #privmsg{from=From, target=Target, message="Morning" ++ K};
	afternooning -> sendPid ! #privmsg{from=From, target=Target, message="Afternooning" ++ K}
    end,
    {ok, State};
handle_event(#privmsg{target=Target, from=From, message="#NING" ++ K}, State) -> 
    Ning = get_ning(erlang:localtime()),
    case Ning of
	evening -> sendPid ! #privmsg{from=From, target=Target, message="EVENING" ++ K};
	morning -> sendPid ! #privmsg{from=From, target=Target, message="MORNING" ++ K};
	afternooning -> sendPid ! #privmsg{from=From, target=Target, message="AFTERNOONING" ++ K}
    end,
    {ok, State};
handle_event(_Msg, State) -> 
    {ok, State}.

terminate(_Args, _State) ->
    ok.

handle_info({'EXIT', _PID, _Reason}, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_ning({_Date, {Hour, _Min, _Sec}}) when Hour < 4 orelse Hour > 17 ->
    evening;
get_ning({_Date, {Hour, _Min, _Sec}}) when Hour >= 4 andalso Hour =< 12 ->
    morning;
get_ning({_Date, {Hour, _Min, _Sec}}) when Hour > 12 ->
    afternooning.



    
