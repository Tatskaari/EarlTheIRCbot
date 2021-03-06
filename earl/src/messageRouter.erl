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

parse(PluginsNames) ->
    receive
	die ->
	    io:format("parserPid :: EXIT~n"),
	    lists:foreach(fun(Name) -> gen_event:delete_handler(irc_messages, Name, []) end, PluginsNames),
	    exit(self(), normal);

	% Reload plugin...
	%#reloadPlugin{name=Name} ->
	%    true;
						
        % deal with registerPlugin requests by adding them to the chan list
	#registerPlugin{name=Name} ->
	    ?MODULE:parse([load(Name)|PluginsNames]);
	
	% deregister plugins
	#deregisterPlugin{name=Name} ->
	    ?MODULE:parse(unload(Name, PluginsNames));
	
	T->
	    Line = lineParse(T),
	    gen_event:notify(irc_messages, Line),

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
		    lists:foreach(ListPlugins, PluginsNames);
		
	        % We don't know about everything - let's not deal with it.	
		_Default -> false 
	    end
    end,
    ?MODULE:parse(PluginsNames).


load(Name) ->
    io:format("adding plugin '~s'~n", [Name]),
    NameAtom = list_to_atom(Name),
    gen_event:add_handler(irc_messages, NameAtom, []),
    Name.

unload(Name, PluginsNames) ->
    io:format("UNLOADING MODULE : ~s~n", [Name]),
    F = fun(N) ->
		if
		    N == Name -> 
			% Remove the event
			gen_event:delete_handler(irc_messages, list_to_atom(Name), []),
			% Remove from the list of plugins
			?MODULE:parse(PluginsNames -- [Name]);
		    true ->
			false
		end
	end,
    lists:foreach(F, PluginsNames),
    PluginsNames.
