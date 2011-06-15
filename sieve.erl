%%%-------------------------------------------------------------------
%%% @author Hunter Kelly <retnuh@gmail.com>
%%% @copyright (C) 2011, Hunter Kelly
%%% @doc
%%% A version of Euler's the Seive of Eratosthenes (see
%%% http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).  By using actors
%%% we can have a "prime server" that computes primes on demand -
%%% kind of like a lazy-seq in clojure or something, but one central dude.
%%%
%%% Technically it may not be the Sieve as defined, because I'm
%%% computing values on demand. Rather than crossing off numbers from
%%% an existing list, I build a list of functions that I then test the next
%%% candidate number (next odd number after current known prime) against.
%%%
%%% The functions created are simple filter functions (i.e. x rem 2 ==
%%% 0).  I reckon that the list of primes will grow a lot more slowly
%%% than taking a big list of numbers, and doing the various sieve
%%% things to it.
%%%
%%% @end
%%% Created : 13 Jun 2011 by Hunter Kelly <retnuh@gmail.com>
%%% -------------------------------------------------------------------
-module(sieve).

-export([start/0, init/0, stop/0, primes_upto/1, first_n/1, next_prime/1, nth_prime/1]).

-include_lib("eunit/include/eunit.hrl").

start() ->
    Started = lists:member(?MODULE, registered()),
    ?debugFmt("Start - registered: ~p", [Started]),
    start(Started).

start(true) ->
    ok;
start(false) ->
    Pid = spawn_link(?MODULE, init, []),        % should this be linked??
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! {self(), stop},
    receive
        V -> V
    end,
    Started = lists:member(?MODULE, registered()),
    ?debugFmt("Stop - registered: ~p ~p", [Started, V]).

% Return a list of primes up to and including Max (if it's a prime).
primes_upto(Max) ->
    ?MODULE ! {self(), upto, Max},
    receive
        {ok, Primes} -> Primes
    end.

% Return a list of the first N primes
first_n(N) ->
    ?MODULE ! {self(), first_n, N},
    receive
        {ok, Primes} ->
            Primes
    end.

%% Return the next Prime greater than N.  N need not be a prime itself
%% (but it is faster if it is).
next_prime(N) ->
    ?MODULE ! {self(), next, N},
    receive
        {ok, Prime} ->
            Prime
    end.

%% Return the nth Prime.
nth_prime(N) ->
    ?MODULE ! {self(), nth, N},
    receive
        {ok, Prime} ->
            Prime
    end.

init() ->
    Dict = dict:store(2, 3, dict:new()),
    Queue = queue:from_list([2,3]),
    loop(Dict, Queue, 3, [make_filter(3)]).

loop(Dict, Queue, HighestPrime, Filters) ->
    receive
        {Pid, stop} ->
            Pid ! unregister(?MODULE),
            ?debugFmt("Stop (server) - registered: ~p", [?debugVal(lists:member(sieve, registered()))]);
        {Pid, upto, Max} ->
            {D, Q, H, F} = find(Dict, Queue, HighestPrime, Filters, Max),
            Primes = queue:filter(fun(P) -> P =< Max end, Q),
            Pid ! {ok, queue:to_list(Primes)},
            loop(D, Q, H, F);
        {Pid, first_n, N} ->
            {D, Q, H, F} = ensure_n(Dict, Queue, HighestPrime, Filters, N),
            {Primes, _} = queue:split(N, Q),
            Pid ! {ok, queue:to_list(Primes)},
            loop(D, Q, H, F);
        {Pid, nth, N} ->
            {D, Q, H, F} = ensure_n(Dict, Queue, HighestPrime, Filters, N),
            {Primes, _} = queue:split(N, Q),
            Pid ! {ok, queue:get_r(Primes)},
            loop(D, Q, H, F);
        {Pid, next, N} ->
            {D, Q, H, F} = find(Dict, Queue, HighestPrime, Filters, N + 2),
            Val = case dict:find(N, D) of
                      {ok, V} ->
                          V;
                      error ->
                          queue:get(queue:filter(fun(P) -> P > N end, Q))
                  end,
            Pid ! {ok, Val},
            loop(D, Q, H, F)
    end.

% Make sure that there are at least N primes
ensure_n(Dict, Queue, HighestPrime, Filters, N) when length(Filters) >= N-1 ->
    {Dict, Queue, HighestPrime, Filters};
ensure_n(Dict, Queue, HighestPrime, Filters, N) ->
    {D, Q, H, F} = find(Dict, Queue, HighestPrime, Filters, HighestPrime + 2),
    ensure_n(D, Q, H, F, N).

% Make sure all the primes upto Max have been found.
find(Dict, Queue, HighestPrime, Filters, Max) when Max =< HighestPrime ->
    {Dict, Queue, HighestPrime, Filters};
find(Dict, Queue, HighestPrime, Filters, Max) ->
    Next = find_next(HighestPrime, Filters),
%    ?debugFmt("Finding next prime: ~p ~p ~p", [HighestPrime, Next, Max]),
    find(dict:store(HighestPrime,Next, Dict), queue:in(Next, Queue), Next,
         [make_filter(Next) | Filters], Max).

make_filter(N) ->
%    ?debugFmt("Making filter for ~p", [N]),
    fun(X) -> X rem N /= 0 end.

find_next(Start, Filters) ->
    Next = Start + 2,
    case lists:all(fun(F) -> F(Next) end, Filters) of
        true ->
            Next;
        _ ->
            find_next(Next, Filters)
    end.
    
primes_test_() ->
    {setup, fun() -> start() end, fun(_) -> stop() end,
     [
      ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], primes_upto(30)),
      ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19], primes_upto(20)),
      ?_assertEqual([2, 3], first_n(2)),
      ?_assertEqual([2], first_n(1)),
      ?_assertEqual([2, 3, 5, 7, 11], first_n(5)),
      ?_assertEqual(3, next_prime(2)),
      ?_assertEqual(2, next_prime(-1)),
      ?_assertEqual(23, next_prime(19)),
      ?_assertEqual(23, next_prime(20))
     ]}.

nth_primes_test_() ->
    {setup, fun() -> start() end, fun(_) -> stop() end,
     [
      ?_assertEqual(2, nth_prime(1)),
      ?_assertEqual(3, nth_prime(2)),
      ?_assertEqual(17, nth_prime(7)),
      ?_assertEqual(31, nth_prime(11))
     ]}.

next_primes_test_() ->
    {setup, fun() -> start() end, fun(_) -> stop() end,
     [
      ?_assertEqual(3, next_prime(2)),
      ?_assertEqual(5, next_prime(3)),
      ?_assertEqual(23, next_prime(19)),
      ?_assertEqual(29, next_prime(25))
     ]}.
