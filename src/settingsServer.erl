-module(settingsServer).
-behaviour(gen_server).
-export([start_link/0, start_link/1]).
-export([getValue/2, setValue/3, stop/1, getKeys/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, code_change/3, terminate/2]).
-include("earl.hrl").
%-include_lib("eunit/include/eunit.hrl").
%-include_lib("settingsServer_test.erl").


start_link() ->
    gen_server:start_link(settingsServer, {}, []).

start_link(FileName) ->
	gen_server:start_link(settingsServer, {FileName}, []).

init({}) ->
	State = dict:new(),
	{ok, {State, false}};

init({FileName}) ->
	case file:read_file(FileName) of
		{ok, Bin} -> State = binary_to_term(Bin);
		_ -> State = dict:new()
	end,
	{ok, {State, FileName}}.

stop(Server) ->
	gen_server:cast(Server, stop).

getValue(Server, Name) ->
	gen_server:call(Server, {getSetting, Name}).

setValue(Server, Name, Value) ->
	gen_server:call(Server, {setValue, Name, Value}).

getKeys(Server) ->
	gen_server:call(Server, getkeys).

terminate(normal, _State) ->
    ok.

handle_call(getkeys, _From, {Dict, FileName}) ->
	Ret = dict:fetch_keys(Dict),
	{reply, Ret, {Dict, FileName}};


handle_call({getSetting, Name}, _From, {Dict, FileName}) ->
	case dict:is_key(Name, Dict) of
	true -> Ret =  dict:fetch(Name, Dict);
	false -> Ret = undef
	end,
	{reply, Ret, {Dict, FileName}};

handle_call({setValue, Name, Value}, _From, {Dict, FileName}) ->
	NewDict = dict:store(Name, Value, Dict),
	case FileName of
		false->false;
		F -> file:write_file(F, term_to_binary(NewDict))
	end,
	{reply, true, {NewDict, FileName}}.


handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
