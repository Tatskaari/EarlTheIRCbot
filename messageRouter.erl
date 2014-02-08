-module(messageRouter).
-export([parse/0, parse/1]).
-import(ircParser, [lineParse/1]).
-include_lib("eunit/include/eunit.hrl").

-include("earl.hrl").

%Contains the record definitions
-include("ircParser.hrl").


% Starts passing the message around to the different handlers.
parse() ->
	parse([]).

parse(PluginsChans) ->
    receive
		die ->
			io:format("parserPid :: EXIT~n"),
			lists:foreach(fun({Pid,_}) -> Pid ! die end, PluginsChans),
			exit(self(), normal);

    	% deal with registerPlugin requests by adding them to the chan list
		#registerPlugin{name=Name} ->
			io:format("adding plugin"),
			NameAttom = list_to_atom(Name),
			Chan = spawn(NameAttom, NameAttom, []),
			?MODULE:parse([{Chan,Name}|PluginsChans]);

		% deregister plugins
		#deregisterPlugin{name=Name} ->
			io:format("UNLOADING MODULE : ~s~n", [Name]),
			F = fun({Chan, N}) ->
					case {Chan, N} of
						{Chan, Name} ->
							Chan ! die,
							?MODULE:parse(PluginsChans -- [{Chan, Name}]);
						_Default -> false 
					end
				end,
			lists:foreach(F, PluginsChans);


		T->
			Line = lineParse(T),
			case Line of
				{} -> false;
				_A ->
					% Anonnomous function (F) to send line to every registered plugin
					F = fun({Chan, _}) -> Chan ! Line end,
					% For each plugin run F against it
					lists:foreach(F, PluginsChans),

					% Built in commands which are required for the protocol
					case Line of
						% Ping
						#ping{nonce=K} ->
							sendPid ! #pong{nonce=K};

						#privmsg{from=From, target=To, message="#plugins"} ->
							io:format("~p~p~n~p~n", [To, From, Line]),
							ListPlugins = fun(Chan) ->
								M = io_lib:format("~p", [Chan]),
								sendPid ! #privmsg{target=To, message=("Plugin: " ++ M)}
							end,
							lists:foreach(ListPlugins, PluginsChans);

						% We don't know about everything - let's not deal with it.	
						_Default -> false 
					end
			end
    end,
    ?MODULE:parse(PluginsChans).