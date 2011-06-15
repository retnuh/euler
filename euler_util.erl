-module(euler_util).

-export([sqrt_int/1, is_palindrome/1, pow/2, factorial/1]).

-include_lib("eunit/include/eunit.hrl").


% Okay math:sqrt doesn't handle bigints, so I need a sqrt_int fun, that finds the closest integer
% of the sqrt of a number (since any factor of a number can't be > sqrt(N)).

sqrt_int(0) ->
    0;
sqrt_int(1) ->
    1;
sqrt_int(N) ->
    sqrt_int(N, N/2).

sqrt_int(N, Cur) when abs(N - (Cur * Cur)) =< 0.01 ->
%    ?debugFmt("~p ~p", [N, Cur]),
    round(Cur);
sqrt_int(N, C) ->
    Quot = N / C,
%    ?debugFmt("~p ~p ~p", [N, C, Quot]),
    sqrt_int(N, (C + Quot) / 2).

sqrt_int_test_() ->
    [?_assertEqual(1, sqrt_int(2)),
     ?_assertEqual(3, sqrt_int(9)),
     ?_assertEqual(3, sqrt_int(10)),
     ?_assertEqual(1000000, sqrt_int(1000000000000))].

is_palindrome(Int) when is_integer(Int) ->
    is_palindrome(integer_to_list(Int));
is_palindrome(String) when is_list(String), length(String) == 1 ->
    true;
is_palindrome(String) when is_list(String) ->
    Q = queue:from_list(String),
    is_palindrome(Q, queue:get(Q), queue:get_r(Q)).
is_palindrome(_Queue, First, Last) when First /= Last ->
    false;
is_palindrome(Queue, _, _) ->
    case queue:len(Queue) of
        0 ->
            true;
        1 ->
            true;
        _ ->
            is_palindrome(queue:drop(queue:drop_r(Queue)), queue:get(Queue), queue:get_r(Queue))
    end.

palindrome_test_() ->
    [?_assert(is_palindrome(1)),
     ?_assert(is_palindrome("1")),
     ?_assert(is_palindrome(1001)),
     ?_assert(is_palindrome("1001")),
     ?_assert(is_palindrome(10101)),
     ?_assert(is_palindrome("10101")),
     ?_assertNot(is_palindrome(12)),
     ?_assertNot(is_palindrome("12")),
     ?_assertNot(is_palindrome(1002)),
     ?_assertNot(is_palindrome("1002")),
     ?_assertNot(is_palindrome(10102)),
     ?_assertNot(is_palindrome("10102"))].

pow(X, N) ->
    pow(X, N, 1).

pow(_X, 0, A) ->
    A;
pow(X, N, A) ->
    pow(X, N-1, X*A).


pow_test_() ->
    [?_assertEqual(1024, pow(2, 10)),
     ?_assertEqual(81, pow(3, 4))
     ].

factorial(N) ->
    factorial(N, 1).

factorial(0, A) ->
    A;
factorial(N, A) ->
    factorial(N-1, N*A).

factorial_test_() ->
    [?_assertEqual(6, factorial(3)),
     ?_assertEqual(120, factorial(5))
     ].
