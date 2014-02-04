-module(optimusPrime).
-export([optimusPrime/0, primesTo/2, isPrime/2]).

%Contains the record definitions
-include("ircParser.hrl").

%%% IRC part here, maths below %%%
optimusPrime() ->
	receive
		die ->
			io:format("primePid :: EXIT~n"),
			exit(self(), normal);
		#privmsg{target=Target, from=From, message="#primesTo " ++ K} ->
			spawn(optimusPrime, primesTo, [send, {K, From, Target}]);	
		#privmsg{target=Target, from=From, message="#isPrime " ++ K} ->
			spawn(optimusPrime, isPrime, [send, {K, From, Target}])
	end,
	?MODULE:optimusPrime().


% The entry point of the porgram
primesTo(send, {K, From, Target}) ->
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
			case Target of
				"#" ++ _ ->
					sendPid ! {command, {"PRIVMSG", Target, PrintTerm}};
				_ ->
					sendPid ! {command, {"PRIVMSG", From, PrintTerm}}
			end.

isPrime(send, {K, From, Target}) ->
	N = listToNum(K),
			Result = if
				N<0 ->
					"Input Error";
				N>1000000000 ->
					"Input too large";
				true ->
					isPrime(N)
			end,
			if
				Result == true ->
					PrintTerm = From ++ ": " ++ K ++ " is prime";
				true ->
					PrintTerm = From ++ ": " ++ K ++ " is devisable by " ++ io_lib:format("~p",[Result])
			end,
			case Target of
				"#" ++ _ ->
					sendPid ! {command, {"PRIVMSG", Target, PrintTerm}};
				_ ->
					sendPid ! {command, {"PRIVMSG", From, PrintTerm}}
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
isPrime(N) when N < 1 -> false;
isPrime(1) -> false;
isPrime(2) -> true;
isPrime(3) -> true;
isPrime(N) when N rem 2 == 0 -> 2;
isPrime(N) -> notDevisableBy(N, 3).
