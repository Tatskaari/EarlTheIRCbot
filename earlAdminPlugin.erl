-module(earlAdminPlugin).
-behaviour(gen_event).
-include("ircParser.hrl").
-export([init/1, handle_event/2, terminate/2]).
-export([code_change/3, handle_info/2]). 

init(_Args) ->
	{ok, []}.

handle_event(#privmsg{admin=true, message="#n " ++ Nick}, State) ->
	sendPid ! #nick{nick=Nick},
	{ok, State};

handle_event(#privmsg{admin=true, message="#j " ++ K}, State) ->
	io:format("Joining '~s'~n", [K]),
	sendPid ! #join{channel=K},
	{ok, State};

% Join (#j <CHANNEL>)
handle_event(#privmsg{admin=true, message="#j " ++ K}, State) ->
	io:format("Joining '~s'~n", [K]),
	sendPid ! #join{channel=K},
	{ok, State};
		
% Part (#p <CHANNEL>)
handle_event(#privmsg{admin=true, message="#p " ++ Channel}, State) ->
	sendPid ! #part{channel=Channel},
	{ok, State};

% Quit (#q [reason])
handle_event(#privmsg{admin=true, message="#q " ++ K}, State) ->
	sendPid ! #quit{reason=K},
	{ok, State};

handle_event(#privmsg{admin=true, message="#q"}, State) ->
	sendPid ! #quit{reason="Earl Out"},
	{ok, State};

% Unloads modules
handle_event(#privmsg{admin=true, from=From, target=To, message="#unload earlAdminPlugin"}, State) ->
	sendPid ! #privmsg{from=From, target=To, message=From ++ ": Unloading earlAdminPl... Wait a second. What the hell are you doing?"},
	{ok, State};

% Unloads modules
handle_event(#privmsg{admin=true, from=From, target=To, message="#unload " ++ ModuleName}, State) ->
	parserPid ! #deregisterPlugin{name=ModuleName},
	sendPid ! #privmsg{from=From, target=To, message=From ++ ": Unloaded " ++ ModuleName},
	{ok, State};

% Loads modules
handle_event(#privmsg{admin=true, from=From, target=To, message="#load " ++ ModuleName}, State) ->
	parserPid ! #registerPlugin{name=ModuleName},
	sendPid ! #privmsg{from=From, target=To, message=From ++ ": loaded " ++ ModuleName},
	{ok, State};

handle_event(_Msg, State) ->
	{ok, State}.

terminate(_Args, _State) ->
    ok.


handle_info({'EXIT', _Pid, _Reason}, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
