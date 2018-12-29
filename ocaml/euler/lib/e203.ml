
(* 
Pascal's triangle - first row is 1, 2nd is 1 1, etc.  However, choose n k - n corresponds to n+1th row of pascals triangle.
So in problem it asks for 51st row, that will be up to choose 50 25 etc.
*)
open Core_kernel

(* open Numbers *)

module IntSet = Set.Make(Int)

let row_from_previous_row =
  let rec aux acc prev = function
    | [] -> acc
    | [x] -> x + 1 :: prev + x :: acc
    | x :: xs -> aux (prev + x :: acc) x xs
  in aux [] 1

let%expect_test _ = 
  let ans = row_from_previous_row [3; 3]
  in ans |> List.map ~f:string_of_int |> String.concat ~sep:" " |> print_endline;
  [%expect{|
    4 6 4
   |}]

let distinct_elts_through_row rows = 
  let rec aux acc prev_row c =
    if c <= 0 then acc |> IntSet.to_list |> List.sort ~compare:(Fn.flip Int.compare) else
      let row = row_from_previous_row prev_row in
      let nacc = List.fold row ~init:acc ~f:IntSet.add in
      aux nacc row (c-1)
  in aux (IntSet.of_list [1; 2]) [2] (rows - 3)

let squares_of_primes_upto upto =
  Sieve.primes_fold_upto ~init:[] ~f:(fun a p -> (p*p)::a) upto |> List.rev

let rec squarefree n = function
  | [] -> 
    (* Printf.printf "Hmm out of primes?  %d\n" n; *)
    true
  | p2 :: _ when p2 > n -> true
  | p2 :: _ when p2 = n -> 
    (* Printf.printf "not squarefree: %d %d\n" p2 n;  *)
    false
  | p2 :: xs -> if n mod p2 = 0 then (
      (* Printf.printf "not squarefree: %d %d\n" p2 n; *)
      false )
    else squarefree n xs

let e203 rows = 
  let elts = distinct_elts_through_row rows in
  let squares = squares_of_primes_upto rows in 
  (* elts |> List.sexp_of_t Int.sexp_of_t |> Sexp.to_string |> Printf.printf "elts: %s\n"; *)
  let filtered = List.filter elts ~f:(fun e ->  squarefree e squares) in
  (* filtered |> List.sexp_of_t Int.sexp_of_t |> Sexp.to_string |> Printf.printf "elts: %s\n"; *)
  List.fold filtered ~init:0 ~f:(fun a e -> a + e)

let%test _ = (* Printf.printf "e203 8: %s\n" @@ string_of_int @@ e203 8; *) e203 8 = 105


(* let distinct_elts_through_row row =
   let one_row acc r = 
    let ans = List.fold (List.range ~stop:`inclusive 1 (chalf r)) ~init:acc ~f:(fun a x -> IntSet.add a @@ choose r x) in
    (* ans |> IntSet.sexp_of_t |> Sexp.to_string |> Printf.printf "one_row: %d %d %s\n" (chalf r) r; *)
    ans
   in let rec aux acc = function
      | 1 | 0 -> Core.Set.add acc 1 |> IntSet.to_list |> List.sort ~compare:(Fn.flip Int.compare)
      | x -> aux (one_row acc x) (x - 1)
   in aux IntSet.empty (row-1) *)

let%expect_test _ =
  let ans = distinct_elts_through_row 8
  in ans |> List.map ~f:string_of_int |> String.concat ~sep:" " |> print_endline;
  [%expect{|
   35 21 20 15 10 7 6 5 4 3 2 1
   |}]

let%test _ = distinct_elts_through_row 5 = [6; 4; 3; 2; 1]

let run_default = 51
let run n = e203 n |> string_of_int |> print_endline

(* 
Answer: limit: 126410606437752 11243248
Stopping at prime: 11243291 126411592510681
34029210557338
Execution time: 20.883102s


However, steve has a comment:
; there are 614 distinct numbers. the largest is 126410606437752
; from the binomial theorem it is 50! / 25! 25!
; therefore the maximum square prime we need consider is 49

I don't get that.  Checking the rejected ones, however, 49 is highest.

Comment from forums:

"Any prime dividing C(n,k) is ≤ n, so you need only the primes up to 47."

Of course!  we know what the possible primes are, because it's factorial.  Derp.
Revised to use more efficient primes:

Answer: highest p^2: 2209
34029210557338
Execution time: 0.002623s

Getting rid of zarith since fits in 64 bit ints:
shiv:~/Dropbox/projects/clojure/euler/ocaml/euler $ dune exec bin/euler.exe 51
Answer: highest p^2: 2209
34029210557338
Execution time: 0.001452s
 *)