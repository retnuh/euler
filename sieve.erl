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

%% TODO this is bad
-compile(export_all).
%-export([start/0, init/0, stop/0, primes_upto/1, first_n/1, first_prime/0, next_prime/1, nth_prime/1]).

-include_lib("eunit/include/eunit.hrl").

-define(CHUNK, 65536).  % Size to "grow" by

start() ->
    Started = lists:member(?MODULE, registered()),
%    ?debugFmt("Start - registered: ~p", [Started]),
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
    end.
%    ?debugFmt("Stop - registered: ~p ~p", [lists:member(?MODULE, registered()), V]).

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

%% Return the first prime and a token to iterate subsequent primes.
first_prime() ->
    ?MODULE ! {self(), first},
    receive
        {ok, Prime, Token} ->
            {Prime, Token}
    end.

%% Return the next Prime in the sequence represented by Token
next_prime(Token) ->
    ?MODULE ! {self(), next, Token},
    receive
        {ok, Prime, T} ->
            {Prime, T}
    end.

%% Return the nth Prime.
nth_prime(N) ->
    ?MODULE ! {self(), nth, N},
    receive
        {ok, Prime} ->
            Prime
    end.

init() ->
    {Set, Highest} = ensure_highest(gb_sets:singleton(2), 2, 100),
    loop(Set, Highest).

loop(Set, Highest) ->
    receive
        {Pid, stop} ->
            Pid ! unregister(?MODULE);
%            ?debugFmt("Stop (server) - registered: ~p", [?debugVal(lists:member(sieve, registered()))]);
        {Pid, upto, Max} ->
            {S, H} = ensure_highest(Set, Highest, Max),
            Primes = gb_sets:filter(fun(P) -> P =< Max end, S),
            Pid ! {ok, gb_sets:to_list(Primes)},
            loop(S, H);
        {Pid, first_n, N} ->
            {S, H} = ensure_n_primes(Set, Highest, N),
            Pid ! {ok, take(N, S)},
            loop(S, H);
        {Pid, nth, N} ->
            {S, H} = ensure_n_primes(Set, Highest, N),
            {Val, _} = iter(N, gb_sets:iterator(S)),
            Pid ! {ok, Val},
            loop(S, H);
        {Pid, first} ->
            {First, Iter} = gb_sets:next(gb_sets:iterator(Set)),
            Pid ! {ok, First, {2, gb_sets:size(Set), Iter}},
            loop(Set, Highest);
        {Pid, next, Token} ->
            {N, Size, I1} = Token,
            {S1, H1} = case (Size - N) of
                           X when X >= 0 ->
                               {Val, I2} = gb_sets:next(I1),
                               Pid ! {ok, Val, {N+1, Size, I2}},
                               {Set, Highest};
                           _ ->
                               %% Iterator doesn't go far enough
                               {S, H} = ensure_n_primes(Set, Highest, N),
                               {Val, Iter} = iter(N, gb_sets:iterator(S)),
                               Pid ! {ok, Val, {N+1, gb_sets:size(S), Iter}},
                               {S, H}
                       end,
            loop(S1, H1)
    end.

take(N, S) ->
    lists:reverse(take(N, gb_sets:iterator(S), [])).
take(0, _, List) ->
    List;
take(N, Iter, List) ->
    {Val, I2} = gb_sets:next(Iter),
    take(N-1, I2, [Val | List]).

iter(1, I1) ->
    gb_sets:next(I1);
iter(N, I1) ->
    {_, I2} = gb_sets:next(I1),
    iter(N - 1, I2).

% Make sure that there are at least N primes
ensure_n_primes(Set, Highest, N) ->
    case gb_sets:size(Set) - N of
        X when X >= 0 ->
            {Set, Highest};
        _ ->
            {S, H} = ensure_highest(Set, Highest, Highest + ?CHUNK),
            ensure_n_primes(S, H, N)
    end.

% Make sure all the primes upto Desired have been found.
ensure_highest(Set, Current, Desired) when Desired =< Current ->
    {Set, Current};
ensure_highest(Set, Current, Desired) ->
    Start = case Current rem 2 of
                0 -> Current + 1;
                1 -> Current + 2
            end,
    % Skip the first one (2) since we don't generate even numbers
    Candidates = apply_existing_sieve(gb_sets:next(element(2, gb_sets:next(gb_sets:iterator(Set)))),
                                      lists:seq(Start, Desired, 2)),
    S = sieve_candidates(Set, Candidates, euler_util:sqrt_int(Desired)),
    {S, Desired}.

apply_existing_sieve(none, List) ->
    List;
apply_existing_sieve({Prime, Iter}, List) ->
    apply_existing_sieve(gb_sets:next(Iter), lists:filter(fun(X) -> X rem Prime /= 0 end, List)).

sieve_candidates(Set, [Prime | Rest], Stop) when Prime > Stop ->
    gb_sets:union(gb_sets:add(Prime, Set), gb_sets:from_list(Rest));
sieve_candidates(Set, [], _) ->
    Set;
sieve_candidates(Set, [Prime | Rest], Stop) ->
    sieve_candidates(gb_sets:add(Prime, Set), lists:filter(fun(X) -> X rem Prime /= 0 end, Rest), Stop).


primes_test_() ->
    {setup, fun start/0, fun(_) -> stop() end,
     [
      ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], primes_upto(30)),
      ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19], primes_upto(20)),
      ?_assertEqual([2, 3], first_n(2)),
      ?_assertEqual([2], first_n(1)),
      ?_assertEqual([2, 3, 5, 7, 11], first_n(5))
     ]}.

nth_primes_test_() ->
    {setup, fun start/0, fun(_) -> stop() end,
     [
      ?_assertEqual(2, nth_prime(1)),
      ?_assertEqual(3, nth_prime(2)),
      ?_assertEqual(17, nth_prime(7)),
      ?_assertEqual(31, nth_prime(11))
     ]}.

next_primes_test_() ->
    {setup, fun start/0, fun(_) -> stop() end,
     {generator, ?MODULE, next_primes_test_generator}
    }.

next_primes_test_generator() ->
    next_primes_test_generator(first_prime(), [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]).
next_primes_test_generator(_, []) ->
    [];
next_primes_test_generator({P, T}, [E | Rest]) ->
    {generator, fun() ->
                        [?_assertEqual(E, P),
                         next_primes_test_generator(next_prime(T), Rest)]
                end
    }.
