open Core
open Core_extended
open Core_bench
open Numbers


let e407_orig n =
  let rec m a = 
    if (a*a) mod n = a then
      a
    else
      m (a-1)
  in
  m (n-1)


(* let reverse_sort = List.sort ~cmp:(Fn.flip compare)  *)

let is_idempotent n a = (a*a) mod n = (n-a)

let check_multiples n x =
  let h = n/2 in
  let rec check a =
    if a > h then
      None
    else if is_idempotent n a then
      Some a
    else
      check (a + x)
  in check x

let e407 n fs =
  let factors = List.map fs ~f:(fun (p,c) -> Int.pow p c) |> List.sort ~compare:(Fn.flip Int.compare) in 
  (* let primes = List.map fs ~f:hd |> reverse_sort in *)
  let highest_factor_m1 = List.hd_exn factors - 1 in
  (* Printf.printf "highest: %d " highest_factor_m1; *)
  (* Try highest factor - 1 *)
  if is_idempotent n highest_factor_m1 then
    n - highest_factor_m1
  else match factors with
    | [] -> failwith "wtf empty factors"
    | f :: fs ->
      let rec check h a x rst =
        if a > h then
          match rst with
          | [] -> h
          | x :: xs -> check h x x xs
        else if is_idempotent n a then
          match rst with
          | [] -> a
          | x :: xs -> check a x x xs
        else
          check h (a + x) x rst
      in n - (check (n/2) f f fs)


let e407_brute n =
  let rec m a = 
    if (a*a) mod n = a then
      a
    else
      m (a-1)
  in
  m (n-1)

let e407_examine n r =
  Printf.printf "%02d %02d:  " n r;
  for i = 1 to (n-1) do
    Printf.printf "%02d " ((i*i) mod n)
  done;
  Printf.printf "\n"

let e407_loop n =
  let seq = Sieve.factors_seq n in
  let f tot = function
    | Zero -> tot
    | One -> tot+1
    | Prime _ -> tot + 1
    | Composite_Factors (x, ((All fs) as f)) ->
      let r = (match fs with
          | _ :: _ :: _tail ->
            let r = e407 x fs in
            Printf.printf "M(%02d) = %02d %02d %s\n" x r (x-r) (string_of_factors f);
            (* e407_examine x r; *)
            r
          | _ -> 1) in

      tot+r
    | _ -> failwith "Unexpected natural number"
  in Lazy_sequence.fold seq ~init:0 ~f:f 

let e407_loop_collect n =
  let seq = Sieve.factors_seq n in
  let f (tot, res) = function
    | Zero -> (tot, res)
    | One -> (tot+1, (1,1) :: res)
    | Prime p -> (tot + 1, (p, 1) :: res)
    | Composite_Factors (x, ((All fs) as f)) ->
      let r = (match fs with
          | _ :: _ :: _tail ->
            let r = e407 x fs in
            Printf.printf "M(%02d) = %02d %s %02d\n" x r (string_of_factors f) x;
            (* e407_examine x r; *)
            r
          | _ -> 1) in

      (tot+r, (x, r) :: res)
    | _ -> failwith "Unexpected natural number"
  in Lazy_sequence.fold seq ~init:(0, []) ~f:f 


let e407_brute n =
  let tot = ref 0 in
  let res = ref [] in 
  for a = 1 to n do
    let r = (e407_brute a) in
    tot := !tot + r;
    res := (a, r) :: !res;
    (* Printf.printf "M(%d) = %d\n" a r *)
    (* e407_examine a r; *)
  done;
  (!tot, !res)

let e407_bench sizes =
  Command.run (Bench.make_command [
      Bench.Test.create_indexed
        ~name:"Brute"
        ~args:sizes
        (fun upto ->
           Staged.stage (fun () -> ignore(e407_brute upto)));
      Bench.Test.create_indexed
        ~name:"Factors"
        ~args:sizes
        (fun upto ->
           Staged.stage (fun () -> ignore(e407_loop upto)));
    ])

let rec diff a b = match (a, b) with
  | ([], []) -> ()
  | (((xa,ha) :: ta), ((xb,hb) :: tb)) -> if not (xa = xb &&  ha = hb) then
      Printf.printf "mismatch: %d %d %d %d\n" xa ha xb hb
    else
      ();
    diff ta tb
  | _ -> ()

let run_old size =
  let (btot, bres) = e407_brute size in
  let (ltot, lres) = e407_loop_collect size in
  diff bres lres;
  Printf.printf "ans = %d %d\n" btot ltot

let run size =
  let ltot = e407_loop size in
  Printf.printf "ans = %d\n" ltot





