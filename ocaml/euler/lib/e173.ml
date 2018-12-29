
(*
1
4
9
9 - 1 = 8
16
16 - 4 = 12
25
25 - 9
25 - 1
36
36 - 16 = 20
36 - 4 = 32
49
49 - 25 = 24
49 - 9 = 40
49 - 1 = 48
64 
64 - 36
64 - 16
64 - 4


*)
let nums_less_then n = 
  if Numbers.is_even n then n / 2 - 1 else (n / 2)

let%test _ = nums_less_then 8 = 3
let%test _ = nums_less_then 9  = 4

let brute_one max n =
  let n_squared = n*n in
  let rec aux tot x = match x with
    | 0 | (-1) -> tot
    | x -> 
      let ntot = (if n_squared - (x*x) <= max then (1 + tot) else tot) in
      if n_squared - (x*x) <= max then 
        (
          (* Printf.printf("\tn: %d x: %d (%d-%d): %d tot: %d ntot: %d\n") n x n_squared (x*x) (n_squared - (x*x)) tot ntot; *)
          aux ntot (x-2)
        )
      else tot
  in 
  aux 0 (n-2)


let%test _ = brute_one 10 3 = 1
let%test _ = brute_one 100 8 = 3


let do_one max n = 
  if (n*n) - ((n-2) * (n-2)) > max then 0 else
  if (n*n) <= max then nums_less_then n else  brute_one max n


let%test _ = do_one 10 3 = 1
let%test _ = do_one 10 8 = 0

let do_upto max =
  let rec aux tot n = 
    let x = do_one max n in
    (* Printf.printf "n: %d x: %d tot: %d\n" n x (tot+x); *)
    if x = 0 then tot else aux (x+tot) (n+1)
  in aux 0 3

let run n = 
  let ans = do_upto n in
  ans |> string_of_int |> print_endline

let%test _ = do_upto 10 = 1
let%test _ = do_upto 25 = 6
let%test _ = do_upto 100 = 41

(*
shiv:~/Dropbox/projects/clojure/euler/ocaml/euler $ dune exec bin/euler.exe
Answer: starting for: 1000000
1572729
Execution time: 0.006339s
 *)