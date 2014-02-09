-module(settingsServer).
-behaviour(gen_server).
-export([start_link/0]).
-export([getValue/2, setValue/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, code_change/3, terminate/2]).
-include("earl.hrl").
%-include_lib("eunit/include/eunit.hrl").
%-include_lib("settingsServer_test.erl").


start_link() ->
    gen_server:start_link(settingsServer, [], []).

init(_Args) ->
    {ok, dict:new()}.

stop(Server) ->
	gen_server:cast(Server, stop).

getValue(Server, Name) ->
	gen_server:call(Server, {getSetting, Name}).

setValue(Server, Name, Value) ->
	gen_server:call(Server, {setValue, Name, Value}).

terminate(normal, _State) ->
    ok.

handle_call({getSetting, Name}, _From, Dict) ->
	case dict:is_key(Name, Dict) of
	true -> Ret =  dict:fetch(Name, Dict);
	false -> Ret = undef
	end,
	{reply, Ret, Dict};
handle_call({setValue, Name, Value}, _From, Dict) ->
	{reply, true, dict:store(Name, Value, Dict)}.


handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
