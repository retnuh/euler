open Core_kernel
open Numbers

let rec endswith a b =
  if b = 0 then true else
    let ad, am = divmod a 10 
    and bd, bm = divmod b 10 
    in if am = bm then endswith ad bd else false

let%test _ = endswith 1219 19 = true
let%test _ = endswith 1219 18 = false
let%test _ = endswith 1219 29 = false

(* Technically this doesn't work for 5, but problem specifically starts from 5; so non-issue *)
let ending_multiple_table = 
  List.fold [9; 7; 5; 3; 1] ~init:[] ~f:(fun acc p ->
      let arr = Array.create ~len:5 0 in
      List.iter [1; 3; 5; 7; 9] ~f:(fun i -> Array.set arr (((i*p) mod 10)/2) i);
      (* arr |> Array.sexp_of_t Int.sexp_of_t |> Sexp.to_string |> Printf.printf("%d: %s\n") p; *)
      arr :: acc
    ) |> Array.of_list

let lookup_ending_multiple p1 p2 = 
  let ones_p1 = p1 mod 10 
  and ones_p2 = p2 mod 10 
  in let arr = Array.get ending_multiple_table (ones_p2/2) in 
  Array.get arr (ones_p1/2)

let%test _ = lookup_ending_multiple 19 23 = 3
let%test _ = lookup_ending_multiple 5 7 = 5
let%test _ = lookup_ending_multiple 11 13 = 7
let%test _ = lookup_ending_multiple 13 17 = 9

let connection p1 p2 = 
  let rec aux m =
    let t = (m*p2) in
    if endswith t p1 then t else aux (m+10)
  in aux @@ lookup_ending_multiple p1 p2

let%test _ = connection 19 23 = 1219
let%test _ = connection 5 7 = 35

let e134 n = 
  Sieve.primes_fold_upto ~start:7 ~init:(0, 5) n ~f:(fun (tot, prev) p -> (tot+connection prev p, p))

let run_default = 100
let run n = 
  let (ans, _last_p) = e134 n in ans

(* 
First pass: shiv:~/Dropbox/projects/clojure/euler/ocaml/euler $ dune exec bin/euler.exe 1000004
Answer: 18613426663617118
Execution time: 204.181386s

Slowish, esp. for ocaml.  Guess it's too brute forcy?
 *)