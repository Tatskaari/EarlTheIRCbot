-module(optimusPrime).
-export([starts/1]).


%%% IRC part here, maths below %%%

% the entry point of the porgram
starts(SendPid) ->
	receive
		[From,_,_,Target,"#primesTo " ++ K] ->
			N = listToNum(K),
			Primes = if
				N<0 ->
					"Input Error";
				true ->
					primesTo(N)
			end,
			PrintTerm =  string:strip(string:strip(io_lib:format("~p",[Primes]), both, $\r), both, $\n),
			io:format("~p~n", [PrintTerm]),
			if
				Target == "Earl2" ->
					SendPid ! {command, {"PRIVMSG", From, PrintTerm}};
				true ->
					SendPid ! {command, {"PRIVMSG", Target, PrintTerm}}
			end	
	end,
	starts(SendPid).

% takes a string and turns it into an integer
listToNum(List) ->
    case string:to_integer(List) of
        {error, _} -> -1;
        {F,_Rest} -> F
    end.

% turns an int into a string so it can be printed
intToString(A) -> 
		lists:flatten(io_lib:format("~p", [A])).




%%% MATHS PAST THIS POINT %%%
% Devides the specified number by evey number below it
sieve(_, []) -> 
	true;
sieve(N, [Head|_]) when ((N rem Head) == 0) -> 
	false;
sieve(N, [_|Tail]) -> 	
	sieve(N, Tail).

% uses the seive method to find every prime below the specified number
primesTo(N) -> 
	primesTo(N, 3, [2]).
primesTo(N, T, Primes) when T > N -> 
	Primes;
primesTo(N,T,Primes) -> 
	case sieve(T,Primes) of
		true -> primesTo(N, T + 2, Primes ++ [T]);
		_ -> primesTo(N, T + 2, Primes)
	end.
