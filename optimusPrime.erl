-module(optimusPrime).
-export([starts/1, sendPrimesTo/4, sendIsPrime/4]).


%%% IRC part here, maths below %%%

% the entry point of the porgram
starts(SendPid) ->
	receive
		die ->
			io:format("primePid :: EXIT~n"),
			exit(self(), normal);
		[From,_,_,Target,"#primesTo " ++ K] ->
			spawn(optimusPrime, sendPrimesTo, [K, From, Target, SendPid]);	
		[From,_,_,Target,"#isPrime " ++ K] ->
			spawn(optimusPrime, sendIsPrime, [K, From, Target, SendPid])			
	end,
	starts(SendPid).

sendPrimesTo(K, From, Target, SendPid) ->
	N = listToNum(K),
			Primes = if
				N<0 ->
					"Input Error";
				N>100 ->
					"Input too large";
				true ->
					primesTo(N)
			end,
			PrintTerm = From ++ ": " ++ io_lib:format("~p",[Primes]),
			if
				Target == "Earl2" ->
					SendPid ! {command, {"PRIVMSG", From, PrintTerm}};
				true ->
					SendPid ! {command, {"PRIVMSG", Target, PrintTerm}}
			end.

sendIsPrime(K, From, Target, SendPid) ->
	N = listToNum(K),
			Result = if
				N<0 ->
					"Input Error";
				N>1000000000 ->
					"Input too large";
				true ->
					isPrime(N)
			end,
			PrintTerm = From ++ ": " ++ io_lib:format("~p",[Result]),
			if
				Target == "Earl2" ->
					SendPid ! {command, {"PRIVMSG", From, PrintTerm}};
				true ->
					SendPid ! {command, {"PRIVMSG", Target, PrintTerm}}
			end.


% takes a string and turns it into an integer
listToNum(List) ->
    case string:to_integer(List) of
        {error, _} -> -1;
        {F,_Rest} -> F
    end.

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
primesTo(N, T, Primes) when T > N-> 
	Primes;
primesTo(N,T,Primes) -> 
	case sieve(T,Primes) of
		true -> primesTo(N, T + 2, Primes ++ [T]);
		_ -> primesTo(N, T + 2, Primes)
	end.

%checks to see if A is devisable by anything below it, starting from the bottom. 
notDevisableBy(A, B) when B>=A -> true;
notDevisableBy(A, B) ->
	if 
		A rem B == 0 ->
			B;
		true ->
			notDevisableBy(A, B+2) %don't need to check even numbers so we increment by 2
	end.

%is N prime
isPrime(1) -> false;
isPrime(2) -> true;
isPrime(3) -> true;
isPrime(N) -> notDevisableBy(N, 3).
