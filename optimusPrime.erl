-module(optimusPrime).
-export([optimusPrime/0, primesTo/2, isPrime/2, get_Integer/1]).

%Contains the record definitions
-include("ircParser.hrl").

% Unit tests
-include_lib("eunit/include/eunit.hrl").

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


% prints all the primes before K
primesTo(send, {K, From, Target}) ->
	N = get_Integer(K),
	Primes = if
		N<0 ->
			"Input Error";
		N>600 ->
			"Input too large";
		true ->
			primesTo(N)
	end,
        if
		Primes == "Input Error" ->
			PrintTerm = From ++ ": " ++ Primes;
		true ->
			PrintTerm = From ++ ": " ++ io_lib:format("~w",[Primes])
	end,
	sendPid ! #privmsg{from=From, target=Target, message=PrintTerm}.

% return trur if K is prime otherwise it returns the lowest factor
isPrime(send, {K, From, Target}) ->
	N = get_Integer(K),
	Result = if
		% A prime number (or a prime) is a natural number greater than
		%   1 that has no positive divisors other than 1 and itself.
		N < 1 ->
			"Input Error";
		N > 1000000000 ->
			"Input too large";
		true ->
			isPrime(N)
	end,
	if
		Result == true ->
			PrintTerm = From ++ ": " ++ K ++ " is prime";
		Result == "Input Error" ->
			PrintTerm = From ++ ": Invalid input.";
		true ->
			PrintTerm = From ++ ": " ++ K ++ " is divisible by " ++ io_lib:format("~p",[Result])
	end,
	sendPid ! #privmsg{from=From, target=Target, message=PrintTerm}.

% Converts a list into a number
% http://stackoverflow.com/questions/4536046/test-if-a-string-is-a-number
get_Integer(S) ->
    try
        K = list_to_integer(S),
        K
    catch error:badarg ->
        -1
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


% =============================================================================
%
%                                  UNIT TESTS
%
% =============================================================================

get_number_test_() ->
	[
		?_assertEqual(
			66,
			get_Integer("66")
		),
		?_assertEqual(
			get_Integer("2746325"),
			2746325
		),
		?_assertEqual(
			get_Integer("This is not a number"),
			-1
		)
	].

get_prime_test_() ->
	[
		?_assertEqual(
			isPrime(7),
			true
		),
		?_assertEqual(
			isPrime(823),
			true
		),
		?_assertEqual(
			isPrime(56),
			2
		),
		?_assertEqual(
			isPrime(9),
			3
		)
	].
