-module(ircSetup).

-behaviour(gen_event).
-include("ircParser.hrl").
-include("earl.hrl").
-export([init/1, handle_event/2, terminate/2]).
-export([code_change/3, handle_info/2]). 


init(_Args) ->
	{ok, {?NICKS, disconnected}}.


handle_event(_A, {Nicks, disconnected}) ->
	sendPid ! #user{user=?USER},
	{ok, {Nicks, userSet}};

handle_event(_A, {[Nick|Nicks], userSet}) ->
	sendPid ! #nick{nick=Nick},
	{ok, {Nicks, nickSent}};

handle_event(#raw{number_code="001"}, _) ->
	JoinChan = fun(Chan) -> sendPid ! #join{channel=Chan} end,
	lists:foreach(JoinChan, ?AUTOJN),
	remove_handler;

handle_event(#raw{number_code="433"},  {[Nick|Nicks], nickSent}) ->
	sendPid ! #nick{nick=Nick},
	{ok, {Nicks, nickSent}};

handle_event(_, State) -> {ok, State}.


terminate(_Args, _State) ->
    ok.


handle_info({'EXIT', _Pid, _Reason}, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
