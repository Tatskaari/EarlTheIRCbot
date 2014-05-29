-module(karma).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).
-export([handle_info/2, code_change/3]).

-include("ircParser.hrl").


init(_Args) ->
    {ok, []}.

handle_event(#privmsg{target=Target, from=From, message="#karma " ++ Name}, State) ->
    {ok, State};
handle_event(#privmsg{target=Target, from=From, message="#karma"}, State) -> 
    {ok, State};
handle_event(#privmsg{target=Target, from=From, message=Name ++ "++"}, State) -> 
    {ok, State};
handle_event(_Msg, State) -> 
    {ok, State}.


