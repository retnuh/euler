
open Core

let e204 h n = 
  let primes = Sieve.primes_upto h 
  and heap = (Heap.create ?min_size:(Some 1000000) ~cmp:Int.compare) () 
  in let rec aux (c: int) prev = 
       match Heap.pop heap with
       | None -> c
       | Some hd ->
         if hd = prev then aux c prev else (
           List.iter primes ~f:(fun p -> 
               let x = hd * p in 
               if x <= n then Heap.add heap x else ());
           aux (c+1) hd
         )
  in 
  Heap.add heap 1;
  aux 0 0 


let%test _ = let ans = e204 5 15 in
  (* Printf.printf "e204 5 15: %d\n" ans;  *)
  ans = 11

let%test _ = let ans = e204 5 400 in 
  (* Printf.printf "e204 5 400: %d\n" ans;  *)
  ans = 61

let run_default = Int.pow 10 9
let run n = e204 100 n

(* 
shiv:~/Dropbox/projects/clojure/euler/ocaml/euler $ dune exec bin/euler.exe
Answer: 2944730
Execution time: 41.212209s
 *)