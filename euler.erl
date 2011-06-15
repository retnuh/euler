-module(euler).

%% TODO write some elisp code to compile & run tests w/out
%% having to leave buffer

%% TODO don't export all
-compile(export_all).
%-export([prob1/1, prob2/1, prob3/1, prob3/3, prob4/1, prob5/1]).

-import(euler_util, [sqrt_int/1, is_palindrome/1, pow/2, factorial/1, choose/2, digits/1, divisors/1]).
-include_lib("eunit/include/eunit.hrl").

-define(time(Expr), timer:tc(fun() -> Expr end, [])).
                                     
%% Add all the natural numbers below one thousand that are multiples of 3 or 5.
prob1(N) ->
    prob1(0, 3, N).

prob1(Sum, Val, N) when Val >= N ->
    Sum;
prob1(Sum, Val, N) when Val rem 3 == 0; Val rem 5 == 0 ->
    prob1(Sum + Val, Val + 1, N);
prob1(Sum, Val, N) ->
    prob1(Sum, Val + 1, N).

prob1_test_() ->
    [?_assertEqual(23, prob1(10)),
     ?_assertEqual(78, prob1(20)),
     ?_assertEqual(98, prob1(21)),
     ?_assertEqual(119, prob1(22))].

%% By considering the terms in the Fibonacci sequence whose values do not exceed four million,
%% find the sum of the even-valued terms.

prob2(N) ->
    prob2(0, 1, 1, N).

prob2(Sum, _Prev, Cur, N) when Cur >= N ->
    Sum;
prob2(Sum, P, C, N) when C rem 2 == 0 ->
    prob2(Sum + C, C, P + C, N);
prob2(S, P, C, N) ->
    prob2(S, C, P + C, N).


%% 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

prob2_test_() ->
    [?_assertEqual(10, prob2(34)),
     ?_assertEqual(44, prob2(35))].

%% The prime factors of 13195 are 5, 7, 13 and 29.
%% What is the largest prime factor of the number 600851475143 ?

prob3(N) ->
    sieve:start(),
    {_Time, List} = timer:tc(?MODULE, prob3, [N, sieve:first_prime(), []]),
%    ?debugFmt("Prob3 time: ~p", [_Time/1000000]),
    lists:sort(List).

prob3(1, _, Factors) ->
    Factors;
prob3(N, {Prime, Tok}, Factors) when N rem Prime == 0 ->
    prob3(N div Prime, {Prime, Tok}, [Prime | Factors]);
prob3(N, {_, T}, F) ->
    prob3(N, sieve:next_prime(T), F).


prob3_test_() ->
    [?_assertEqual([5, 7, 13, 29], prob3(13195)),
     ?_assertEqual([2,2,3,3,5,7,11], prob3(2*2*3*3*5*7*11))].

%% Find the largest palindrome made from the product of two 3-digit numbers.

%% The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.
prob4(Digits) ->
    Start = trunc(math:pow(10, Digits)) - 1,
    prob4(Start, Start, 1, trunc(math:pow(10, Digits - 1))).

prob4(End, _, Highest, End) ->
    Highest;
prob4(Outer, Inner, Highest, End) when Outer * Inner =< Highest; Inner == End ->
    prob4(Outer - 1, Outer - 1, Highest, End);
prob4(O, I, H, E) ->
    Product = O*I,
    case is_palindrome(Product) of
        true ->
            prob4(O-1,O-1,Product, E);
        _ ->
            prob4(O, I-1, H, E)
    end.
           
prob4_test_() ->
    ?_assertEqual(9009, prob4(2)).
    
%% 2520 is the smallest number that can be divided by each of the
%% numbers from 1 to 10 without any remainder.  What is the smallest
%% positive number that is evenly divisible by all of the numbers from
%% 1 to 20?

prob5(N) ->
    FactorCount = prob5_factors(N),
    trunc(orddict:fold(fun(Int, Count, Acc) -> math:pow(Int, Count) * Acc end, 1, FactorCount)).

% return a dict of the form {Integer, MaxCount} for all the ints from 1 to N
% where MaxCount is the maximum times a prime appears in the factorization of
% any one integer (i.e. in prob5_factors(10) there would appear {3, 2} for 9,
% {2, 3} for 8, etc.
prob5_factors(N) ->
    Dict = orddict:new(),
    prob5_factors(Dict, lists:seq(2, N)).
prob5_factors(Dict, []) ->
    Dict;
prob5_factors(Dict, [X | Rest]) ->
    FactorCount = lists:foldl(fun(Factor, Acc) -> orddict:update_counter(Factor, 1, Acc) end,
                              orddict:new(),
                              prob3(X)),
    Merged = orddict:merge(fun(_, A, B) -> max(A, B) end, FactorCount, Dict),
    prob5_factors(Merged, Rest).

prob5_factors_test_() ->
    [?_assertEqual([{2, 2}, {3, 1}, {5, 1}], prob5_factors(5)),
     ?_assertEqual([{2, 3}, {3, 2}, {5, 1}, {7, 1}], prob5_factors(10))].
        
prob5_test_() ->
    ?_assertEqual(2520, prob5(10)).

%% Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.

%% Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

prob6(N) ->
    Seq = lists:seq(1, N),
    {Sum, SumOfSquares} = lists:foldl(fun(X, {S, SoS}) -> {S+X, SoS+(X*X)} end, {0, 0}, Seq),
    (Sum * Sum) - SumOfSquares.
                                       
prob6_test() ->
    ?assertEqual(2640, prob6(10)).

%% By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
%% can see that the 6th prime is 13.  What is the 10001st prime
%% number?

prob7(N) ->
    sieve:start(),
    sieve:nth_prime(N).

% Added after the fact to excercise the sieve after it went kablooie
prob7_test() ->
    ?assertEqual(104743, prob7(10001)).

%% Find the greatest product of five consecutive digits in the 1000-digit number.

-define(PROB8, "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450").

%% Find the greatest product of N Consecutive digits in the given number-string
prob8(N, String) ->
    Digits = lists:map(fun(Char) -> Char - $0 end, String),
    InitList = lists:sublist(Digits, N),
    InitQueue = queue:from_list(InitList),
    ProdFun = fun(X, Acc) -> X*Acc end,
    InitProd = lists:foldl(ProdFun, 1, InitList),
    {_, _, H} = lists:foldl(fun(NewVal, {Queue, Prod, Highest}) ->
                                    {{value, OldestVal}, Q} = queue:out(queue:in(NewVal, Queue)),
                                    NewProd = case OldestVal of
                                                  0 ->
                                                      lists:foldl(ProdFun, 1, queue:to_list(Q));
                                                  _ ->
                                                      (Prod * NewVal) div OldestVal
                                              end,
                                    {Q, NewProd, max(NewProd, Highest)}
                            end,
                            {InitQueue, InitProd, InitProd},
                            lists:nthtail(N, Digits)),
    H.

prob8() ->
    prob8(5, ?PROB8).

prob8_test_() ->
    [?_assertEqual(60, prob8(3, "19023450")),
     ?_assertEqual(729, prob8(3, "999111")),
     ?_assertEqual(729, prob8(3, "111999")),
     ?_assertEqual(6561, prob8(4, "9999111")),
     ?_assertEqual(6561, prob8(4, "119999")),
     ?_assertEqual(6561, prob8(4, "1119999111"))
    ].

%%% By using Euclid's formula about the relationship between PPTs,
%%% I've been able to reduce searching for a PPT that satisfies
%%% a + b + c = x  to looking for two numbers, m > n, and solving for
%%% this instead:  m^2 + mn = x/2

%%% There exists exactly one Pythagorean triplet for which a + b + c =
%%% 1000. Find the product abc.

prob9() ->
    {_Time, List} = ?debugVal(timer:tc(?MODULE, prob9, [1000])),
    lists:foldl(fun(X, A) -> X*A end, 1, List).

prob9(X) when X rem 2 == 0 ->
    {M, N} = prob9(2, 1, X div 2),
    A = (M*M) - (N*N), % 5, 12, 13
    B = 2*M*N,
    C = (M*M) + (N*N),
    lists:sort([A,B,C]).

prob9(M,1,Y) when M*M + M > Y ->
    error;
prob9(M,N,Y) when M*M + M*N == Y ->
    ?debugVal({M,N});
prob9(M,N,Y) when M == N; M*M + M*N > Y ->
%    ?debugFmt("M: ~p N: ~p Y: ~p incM",[M,N,Y]),
    prob9(M+1,1,Y);
prob9(M,N,Y) when M*M + M*N < Y ->
%    ?debugFmt("M: ~p N: ~p Y: ~p incN",[M,N,Y]),
    prob9(M,N+1,Y).

prob9_test_() ->
    [?_assertEqual([7, 24, 25], prob9(56)),
     ?_assertEqual([5, 12, 13], prob9(30))].


%% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
%% Find the sum of all the primes below two million.

prob10(N) ->
    sieve:start(),
    lists:sum(sieve:primes_upto(N)).

prob10() ->
    timer:tc(?MODULE, prob10, [2000000]).

prob10_test() ->
    ?assertEqual(17, prob10(10)).

%%% Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.

prob13() ->
    L = [37107287533902102798797998220837590246510135740250,
         46376937677490009712648124896970078050417018260538,
         74324986199524741059474233309513058123726617309629,
         91942213363574161572522430563301811072406154908250,
         23067588207539346171171980310421047513778063246676,
         89261670696623633820136378418383684178734361726757,
         28112879812849979408065481931592621691275889832738,
         44274228917432520321923589422876796487670272189318,
         47451445736001306439091167216856844588711603153276,
         70386486105843025439939619828917593665686757934951,
         62176457141856560629502157223196586755079324193331,
         64906352462741904929101432445813822663347944758178,
         92575867718337217661963751590579239728245598838407,
         58203565325359399008402633568948830189458628227828,
         80181199384826282014278194139940567587151170094390,
         35398664372827112653829987240784473053190104293586,
         86515506006295864861532075273371959191420517255829,
         71693888707715466499115593487603532921714970056938,
         54370070576826684624621495650076471787294438377604,
         53282654108756828443191190634694037855217779295145,
         36123272525000296071075082563815656710885258350721,
         45876576172410976447339110607218265236877223636045,
         17423706905851860660448207621209813287860733969412,
         81142660418086830619328460811191061556940512689692,
         51934325451728388641918047049293215058642563049483,
         62467221648435076201727918039944693004732956340691,
         15732444386908125794514089057706229429197107928209,
         55037687525678773091862540744969844508330393682126,
         18336384825330154686196124348767681297534375946515,
         80386287592878490201521685554828717201219257766954,
         78182833757993103614740356856449095527097864797581,
         16726320100436897842553539920931837441497806860984,
         48403098129077791799088218795327364475675590848030,
         87086987551392711854517078544161852424320693150332,
         59959406895756536782107074926966537676326235447210,
         69793950679652694742597709739166693763042633987085,
         41052684708299085211399427365734116182760315001271,
         65378607361501080857009149939512557028198746004375,
         35829035317434717326932123578154982629742552737307,
         94953759765105305946966067683156574377167401875275,
         88902802571733229619176668713819931811048770190271,
         25267680276078003013678680992525463401061632866526,
         36270218540497705585629946580636237993140746255962,
         24074486908231174977792365466257246923322810917141,
         91430288197103288597806669760892938638285025333403,
         34413065578016127815921815005561868836468420090470,
         23053081172816430487623791969842487255036638784583,
         11487696932154902810424020138335124462181441773470,
         63783299490636259666498587618221225225512486764533,
         67720186971698544312419572409913959008952310058822,
         95548255300263520781532296796249481641953868218774,
         76085327132285723110424803456124867697064507995236,
         37774242535411291684276865538926205024910326572967,
         23701913275725675285653248258265463092207058596522,
         29798860272258331913126375147341994889534765745501,
         18495701454879288984856827726077713721403798879715,
         38298203783031473527721580348144513491373226651381,
         34829543829199918180278916522431027392251122869539,
         40957953066405232632538044100059654939159879593635,
         29746152185502371307642255121183693803580388584903,
         41698116222072977186158236678424689157993532961922,
         62467957194401269043877107275048102390895523597457,
         23189706772547915061505504953922979530901129967519,
         86188088225875314529584099251203829009407770775672,
         11306739708304724483816533873502340845647058077308,
         82959174767140363198008187129011875491310547126581,
         97623331044818386269515456334926366572897563400500,
         42846280183517070527831839425882145521227251250327,
         55121603546981200581762165212827652751691296897789,
         32238195734329339946437501907836945765883352399886,
         75506164965184775180738168837861091527357929701337,
         62177842752192623401942399639168044983993173312731,
         32924185707147349566916674687634660915035914677504,
         99518671430235219628894890102423325116913619626622,
         73267460800591547471830798392868535206946944540724,
         76841822524674417161514036427982273348055556214818,
         97142617910342598647204516893989422179826088076852,
         87783646182799346313767754307809363333018982642090,
         10848802521674670883215120185883543223812876952786,
         71329612474782464538636993009049310363619763878039,
         62184073572399794223406235393808339651327408011116,
         66627891981488087797941876876144230030984490851411,
         60661826293682836764744779239180335110989069790714,
         85786944089552990653640447425576083659976645795096,
         66024396409905389607120198219976047599490197230297,
         64913982680032973156037120041377903785566085089252,
         16730939319872750275468906903707539413042652315011,
         94809377245048795150954100921645863754710598436791,
         78639167021187492431995700641917969777599028300699,
         15368713711936614952811305876380278410754449733078,
         40789923115535562561142322423255033685442488917353,
         44889911501440648020369068063960672322193204149535,
         41503128880339536053299340368006977710650566631954,
         81234880673210146739058568557934581403627822703280,
         82616570773948327592232845941706525094512325230608,
         22918802058777319719839450180888072429661980811197,
         77158542502016545090413245809786882778948721859617,
         72107838435069186155435662884062257473692284509516,
         20849603980134001723930671666823555245252804609722,
         53503534226472524250874054075591789781264330331690],
    Sum = lists:sum(L),
    lists:sublist(integer_to_list(Sum), 10).

%%% If the numbers 1 to 5 are written out in words: one, two, three,
%%% four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in
%%% total.
%%%
%%% If all the numbers from 1 to 1000 (one thousand) inclusive were
%%% written out in words, how many letters would be used?

prob17() ->
    timer:tc(?MODULE, prob17, [1000]).

prob17(N) ->
    lists:sum(lists:map(fun(I) -> prob17_count(I) end, lists:seq(1, N))).
                                
prob17_count(X) ->
    lists:flatlength(prob17_word(X)).

prob17_word(X) ->
    prob17_word(X, 1, []).
prob17_word(0, _, Acc) ->
    ?debugVal(Acc);
prob17_word(X, 1, _) when 10 =< X rem 100, X rem 100 < 20 ->
    % we ignore the acc for goofy 11-19
    Y = X rem 10,
    W = lists:nth(Y+1, ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
                        "seventeen", "eighteen", "nineteen"]),
    prob17_word(X div 100, 3, [W]);
prob17_word(X, 1, Acc) ->
    Y = X rem 10,
    W = prob7_number_word(Y),
    prob17_word(X div 10, 2, [W | Acc]);
prob17_word(X, 2, Acc) ->
    Y = X rem 10,
    W = lists:nth(Y+1, [ "", error, "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]),
    prob17_word(X div 10, 3, [W | Acc]);
prob17_word(X, 3, Acc) ->
    Y = X rem 10,
    W = prob7_number_word(Y),
    And = case lists:flatlength(Acc) of
              0 -> "";
              _ -> "and"
          end,
    L = case Y of
            0 -> [ And | Acc];
            _ -> [W , "hundred" |[ And | Acc]]
        end,
    prob17_word(X div 10, 4, L);
prob17_word(X, 4, Acc) ->
    Y = X rem 10,
    W = prob7_number_word(Y),
    prob17_word(X div 10, 4, [W , "thousand" | Acc]).

prob7_number_word(Y) ->
    lists:nth(Y+1, ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]).

prob17_test_() ->
    [?_assertEqual(6, prob17_count(11)),
     ?_assertEqual(20, prob17_count(911)),
     ?_assertEqual(12, prob17_count(9000)),
     ?_assertEqual(18, prob17_count(9001)),
     ?_assertEqual(32, prob17_count(9911)),
     ?_assertEqual(19, prob17(5))
    ].

%% The following iterative sequence is defined for the set of positive integers:
%% n  n/2 (n is even)
%% n  3n + 1 (n is odd)
%% Using the rule above and starting with 13, we generate the following sequence:
%% 13  40  20  10  5  16  8  4  2  1
%% It can be seen that this sequence (starting at 13 and finishing at
%% 1) contains 10 terms. Although it has not been proved yet (Collatz
%% Problem), it is thought that all starting numbers finish at 1.
%% Which starting number, under one million, produces the longest
%% chain?
%% NOTE: Once the chain starts the terms are allowed to go above one million.

prob14() ->
    timer:tc(?MODULE, prob14, [999999]).

prob14(N) ->
    prob14(N, 2, {1, 1}).

prob14(N, N, T) ->
    {_Count, Num} = ?debugVal(T),
    Num;
prob14(Max, N, T) ->
    prob14(Max, N+1, max(T, {prob14_chainlength(N), N})).

%% memoized, actually slower for first run, but subsequent runs much faster.
%% Probably a tipping point (> 1000000) where it helps even on first run
prob14_chainlength(1) ->
    1;
prob14_chainlength(N) ->
    case erlang:get({'euler:prob14', N}) of
        T when is_integer(T) ->
            T;
        'undefined' ->
            Step = case N rem 2 of
                       0 -> N div 2;
                       _ -> 3*N + 1
                   end,
            V = 1 + prob14_chainlength(Step),
            erlang:put({'euler:prob14', N}, V),
            V
    end.

prob14_test_() ->
    [?_assertEqual(10, prob14_chainlength(13)),
     ?_assertEqual(3, prob14(5))
     ].

%%% What is the sum of the digits of the number 2^1000
prob16() ->
    lists:foldl(fun(C, A) -> C+A-$0 end, 0, integer_to_list(pow(2, 1000))).

%%% Find the sum of the digits in the number 100!
prob20() ->
    lists:foldl(fun(C, A) -> C+A-$0 end, 0, integer_to_list(factorial(100))).
    

%% Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
%% 1634 = 14 + 64 + 34 + 44
%% 8208 = 84 + 24 + 04 + 84
%% 9474 = 94 + 44 + 74 + 44
%% As 1 = 14 is not a sum it is not included.
%% The sum of these numbers is 1634 + 8208 + 9474 = 19316.
%% Find the sum of all the numbers that can be written as the sum of
%% fifth powers of their digits.

prob30(Pow) ->
    %% 2 below is 1 + trunc is ceiling, and the other 1 is basically if
    %% the 9^pow can only grow by X, 10X can contain all the possibilities
    Upper = pow(10, 2 + trunc(math:log10(math:pow(9, Pow)))),
    lists:sum(?debugVal(lists:filter(fun(I) -> prob30_is_sum(Pow, I) end, lists:seq(10, Upper)))).

prob30_is_sum(Pow, I) ->
    Digits = digits(I),
    I == lists:sum(lists:map(fun(D) -> pow(D, Pow) end, Digits)).
                                     
prob30_test_() ->
    [?_assertEqual(19316, prob30(4))
    ].

%%% 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
%%% Find the sum of all numbers which are equal to the sum of the factorial of their digits.

prob34() ->
    %% 2 below is 1 + trunc is ceiling, and the other 1 is basically if
    %% the 9! can only grow by X, 10X can contain all the possibilities
    Upper = pow(10, 2 + trunc(math:log10(factorial(9)))),
    lists:sum(?debugVal(lists:filter(fun(I) -> I == lists:sum(lists:map(fun euler_util:factorial/1, digits(I))) end, lists:seq(10, Upper)))).

%%% The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
%%% Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

prob48() ->
    ?time(prob48(1000) rem pow(10,10)).

prob48(N) ->
    lists:foldl(fun(I, A) -> pow(I, I) + A end, 0, lists:seq(1, N)).
                        
prob48_test() ->
    ?_assertEqual(10405071317, prob48(10)).

%% The sequence of triangle numbers is generated by adding the natural
%% numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6
%% + 7 = 28. The first ten terms would be:
%% 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
%% Let us list the factors of the first seven triangle numbers:
%%  1: 1
%%  3: 1,3
%%  6: 1,2,3,6
%% 10: 1,2,5,10
%% 15: 1,3,5,15
%% 21: 1,3,7,21
%% 28: 1,2,4,7,14,28
%% We can see that 28 is the first triangle number to have over five divisors.
%% What is the value of the first triangle number to have over five hundred divisors?

prob12() ->
    ?time(prob12(500)).

prob12(DivisorCount) ->
    prob12(DivisorCount, 1, 1).

prob12(DC, I, N) ->
    Divisors = divisors(N),
    case length(Divisors) >= DC of
        true -> N;
        _ -> prob12(DC, I+1, N+I+1)
    end.

prob12_test_() ->
    [?_assertEqual(28, prob12(5))].


%%% What is the first term in the Fibonacci sequence to contain 1000 digits?

prob25() ->
    ?time(prob25(1000)).

prob25(Digits) ->
    prob25(Digits, 3, 2, 1).

prob25(D, Fnum, N, P) ->
    case length(integer_to_list(N)) >= D of
        true -> Fnum;
        _ -> prob25(D, Fnum+1, N+P, N)
    end.

prob25_test_() ->
    [?_assertEqual(12, prob25(3))
     ].

%%% Find the maximum total from top to bottom of the triangle below:

prob18_tree() -> [
[75],
[95, 64],
[17, 47, 82],
[18, 35, 87, 10],
[20, 04, 82, 47, 65],
[19, 01, 23, 75, 03, 34],
[88, 02, 77, 73, 07, 63, 67],
[99, 65, 04, 28, 06, 16, 70, 92],
[41, 41, 26, 56, 83, 40, 80, 70, 33],
[41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
[53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
[70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
[91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
[63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
[04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]].

prob18() ->
    ?time(prob18(prob18_tree())).

%% Turns out this is a breadth first problem.  Can just make a new
%% tree with each node being the highest total possible to get there,
%% and then pick the total on the last line...


prob18([H | Rest]) ->
    prob18(H, Rest).

prob18(LastScore, []) ->
    lists:foldl(fun(X, A) -> max(X, A) end, 0, LastScore);
prob18(LastScore, [Cur | Rest]) ->
    prob18(prob18_score([0|LastScore], Cur, []), Rest).

prob18_score([X | []], [Y |[]], Acc) ->
    lists:reverse([X+Y|Acc]);
prob18_score([X1 |[ X2 | Xs]], [Y1 | Ys], Acc) ->
    prob18_score([X2 | Xs], Ys, [max(Y1+X2, Y1+X1) | Acc]).

prob18_test() ->
    ?assertEqual(23, prob18([[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]])).
