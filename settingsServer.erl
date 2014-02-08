-module(settingsServer).
-export([setting_server/0, getSetting/2]).
-include("earl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("settingsServer_test.erl").



getSetting(Pid, Name) ->
	Pid ! #getVal{name=Name, return_chan=self()},
	receive
		#retVal{name=Name, value=X} -> 
			X;
		#noVal{name=Name} -> #noVal{name=Name}
	end.

setting_server() -> setting_server(dict:new()).

setting_server(Dict) ->
	receive
		die ->
			io:format("settings :: EXIT~n"),
			exit(self(), normal);

		#setVal{name=Name, value=Value} -> 
			setting_server(dict:store(Name, Value, Dict));
		#getVal{name=Name, return_chan=Chan} ->
			case dict:is_key(Name, Dict) of
				true ->
					Chan ! #retVal{name=Name, value=dict:fetch(Name, Dict)};
				false ->
					Chan ! #noVal{name=Name}
			end
	end,
	setting_server(Dict).
