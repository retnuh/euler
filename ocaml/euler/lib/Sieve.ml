open Core
open Numbers
open Core_bench
open Core_extended


module Sieve_Heap = struct

  type heap_entry = Comp of int * int | Empty

  type table_entry = Pending of int list | Calculated of Int.t Int.Map.t

  let heap_entry_cmp a b = match (a, b) with
    | _, Empty -> -1
    | Empty, _ -> 1
    | Comp (x, _), Comp (y, _) -> compare x y

  type t = heap_entry Heap.t * table_entry Int.Table.t

  let create upto =
    (* Simple Estimate from https://en.wikipedia.org/wiki/Prime-counting_function *)
    let upto_float = float_of_int upto in
    let estimate = 1.25506 *. upto_float /. (log upto_float) |> Float.round_up |> int_of_float in
    let heap = (Heap.create ?min_size:(Some estimate) ~cmp:heap_entry_cmp) () in
    let table = Int.Table.create ~size:upto () in
    Heap.add heap Empty;
    Hashtbl.set table ~key:1 ~data:(Calculated Int.Map.empty);
    (heap, table)

  let heap_entry_to_list accum = function
    | Empty -> List.rev accum
    | Comp (_, p) -> p :: accum

  let string_of_heap heap =
    Heap.fold heap ~init:[] ~f:heap_entry_to_list
    |> (List.sexp_of_t Int.sexp_of_t)
    |> Sexp.to_string_hum


  let add_prime heap p =
    Heap.add heap (Comp (3*p, p))

  let update_heap heap n =
    let rec aux acc = match Heap.top_exn heap with
      | Comp (x, p) when x <= n  -> 
        (* Printf.printf "Bumping heap entry: %d %d %d\n" n x p; *)
        ignore(Heap.pop_exn heap);
        Heap.add heap (Comp ((x + 2*p), p));
        (* Printf.printf "updatec: %d %d %d %s\n" n x p (((List.to_string l) Int.to_string_hum)); *)
        aux (if x = n then p :: acc else acc)
      | _ ->
        (* Printf.printf "update: %d %s\n" n (((List.to_string l) Int.to_string_hum)); *)
        acc
    in
    aux []

  let is_prime heap n =
    match Heap.top_exn heap with
    | Empty -> true
    | Comp (x, _) when n < x -> (* Printf.printf "comp: %d %d\n" x n; *) true
    | Comp (x, _) when n = x -> false
    | Comp (x, p) ->
      failwithf "Overflow in next: %d %d %d" x p n ()

  let next (heap, table) x =
    (* Printf.printf "next: %d\n" x; *)
    if is_prime heap x then
      begin
        (* Printf.printf "Heap Found prime: %d\n" x;  *)
        add_prime heap x;
        ignore(Hashtbl.add table ~key:x ~data:(Calculated (Int.Map.of_alist_exn [x, 1])));
        Prime x
      end
    else
      begin
        let l = update_heap heap x in
        ignore(Hashtbl.add table ~key:x ~data:(Pending l));
        Composite x
      end

  let factors (_, table) n =
    let incr imap key = Map.change imap key ~f:(fun vo -> match vo with
        | None -> Some 1
        | Some v -> Some (v + 1))
    and product = function
      | [] -> 1
      | [x] -> x
      | x :: xs -> List.fold xs ~init:x ~f:( * )
    in let rec factors x lst =
         let p = product lst in
         let resolved = lookup (x/p) in
         List.fold lst ~init:resolved ~f:incr
    and lookup x = match Hashtbl.find table x with
      | Some (Calculated mp) -> mp
      | Some (Pending lst) ->
        let resolved = factors x lst in
        Hashtbl.set table ~key:x ~data:(Calculated resolved);
        resolved
      | None ->
        assert (is_even x);
        let resolved = lookup (Int.shift_right x 1) in
        let updated = incr resolved 2 in
        Hashtbl.set table ~key:x ~data:(Calculated updated);
        updated
    in
    All (Map.to_alist (lookup n))

  let partial_factors (_, table) n =
    let rec lookup x = match Hashtbl.find table x with
      | Some (Calculated mp) -> Int.Map.keys mp
      | Some (Pending lst) -> lst
      | None ->
        assert (is_even x);
        let resolved = lookup (Int.shift_right x 1) in
        if List.hd resolved = Some 2 then resolved else 2::resolved
    in
    Primes (lookup n)

end


let odd_seq upto =
  let t = Sieve_Heap.create upto in
  let (==>) = Lazy_sequence.(==>) in
  let rec f i () =
    (* Printf.printf "i: %d\n" i; *)
    if i > upto then
      Lazy_sequence.empty
    else
      Sieve_Heap.next t i ==> f (i + 2)
  in
  f 3 ()

let partial_factors_seq upto =
  let t = Sieve_Heap.create upto in
  let (==>) = Lazy_sequence.(==>) in
  let (==>>) = Lazy_sequence.(==>>) in
  let fact x = Composite_Factors (x, Sieve_Heap.partial_factors t x) in
  let rec odds i =
    (* Printf.printf "i: %d\n" i; *)
    if i > upto then
      Lazy_sequence.empty
    else
      Sieve_Heap.next t i ==> fun () -> odds (i + 2)
  in let it = Lazy_sequence.Iterator.create (odds 3) in
  let rec f () = match Lazy_sequence.Iterator.get it with
    | None -> Lazy_sequence.empty
    | Some (Prime p) -> [Prime p; fact (p+1)] ==>> f
    | Some (Composite c) -> [fact c; fact (c+1)] ==>> f
    | _ -> failwith "Illegal state!"
  in
  [Zero; One; Prime 2] ==>> f

let factors_seq upto =
  let t = Sieve_Heap.create upto in
  let (==>) = Lazy_sequence.(==>) in
  let (==>>) = Lazy_sequence.(==>>) in
  let fact x = Composite_Factors (x, Sieve_Heap.factors t x) in
  let rec odds i =
    (* Printf.printf "i: %d\n" i; *)
    if i > upto then
      Lazy_sequence.empty
    else
      Sieve_Heap.next t i ==> fun () -> odds (i + 2)
  in let it = Lazy_sequence.Iterator.create (odds 3) in
  let rec f () = match Lazy_sequence.Iterator.get it with
    | None -> Lazy_sequence.empty
    | Some (Prime p) -> [Prime p; fact (p+1)] ==>> f
    | Some (Composite c) -> [fact c; fact (c+1)] ==>> f
    | _ -> failwith "Illegal state!"
  in
  [Zero; One; Prime 2] ==>> f

let nat_seq upto =
  let o_s = odd_seq upto in
  let it = Lazy_sequence.Iterator.create o_s in
  let (==>>) = Lazy_sequence.(==>>) in
  let rec f () = match Lazy_sequence.Iterator.get it with
    | None -> Lazy_sequence.empty
    | Some (Prime p) -> [Prime p; Composite (p+1)] ==>> f
    | Some (Composite c) -> [Composite c; Composite (c+1)] ==>> f
    | _ -> failwith "Illegal state!"
  in
  [Zero; One; Prime 2] ==>> f

let prime_seq upto =
  let o_s = odd_seq upto in
  let it = Lazy_sequence.Iterator.create o_s in
  let (==>) = Lazy_sequence.(==>) in
  let rec f () = match Lazy_sequence.Iterator.get it with
    | None -> Lazy_sequence.empty
    | Some (Prime p) -> Prime p ==> f
    | Some (Composite _) -> f ()
    | _ -> failwith "Illegal state!"
  in
  Prime 2 ==> f

let primes_fold_upto ?(start=2) ~f upto = 
  let s = prime_seq upto in
  let aux acc = function
    | Prime p ->
      let acc' = if p < start then acc else f acc p in
      if p <= upto then Base.Continue_or_stop.Continue acc' else Base.Continue_or_stop.Stop acc
    | _ -> failwith "Illegal state!"
  in
  Lazy_sequence.fold_until s ~f:aux ~finish:(fun l -> l)

let primes_upto = primes_fold_upto ~f:(Fn.flip List.cons) ~init:[]

let count_primes ?(s_fun=odd_seq) upto =
  let s = s_fun upto in
  let f count = function
    | Prime _p ->
      (* Printf.printf "Heap Found prime: %d\n" _p;   *)
      count + 1
    | _ ->
      count
  in
  Lazy_sequence.fold s ~init:1 ~f:f

let bench sizes =
  Command.run (Bench.make_command [
      Bench.Test.create_indexed
        ~name:"Odd seq"
        ~args:sizes
        (fun upto ->
           Staged.stage (fun () -> ignore(count_primes upto)));
      Bench.Test.create_indexed
        ~name:"Natural Seq"
        ~args:sizes
        (fun upto ->
           Staged.stage (fun () -> ignore(count_primes ~s_fun:nat_seq upto)));
      Bench.Test.create_indexed
        ~name:"Factors Seq"
        ~args:sizes
        (fun upto ->
           Staged.stage (fun () -> ignore(count_primes ~s_fun:factors_seq upto)));
    ])


let run_heap x = Printf.printf "Heap Primes upto %d: %d\n" x (count_primes x)
(* let run () = run_heap 20 *)
(*let run () = bench [1000; 10_000; 100_000; 1_000_000]  *)


(* let run () = run_heap 20 *)

(* let run () =
   let s = factors_seq 50 in
   Lazy_sequence.iteri s ~f:(fun i nat -> Printf.printf "%d %s\n" i (string_of_natural nat)) *)

let run () = bench [1000; 10_000; 100_000; 1_000_000; 10_000_000] 



