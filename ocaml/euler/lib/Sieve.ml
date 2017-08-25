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
    | Comp (x, xp), Comp (y, yp) -> compare x y

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
    let rec acc l = match Heap.top_exn heap with
      | Comp (x, p) when x <= n  -> 
         (* Printf.printf "Bumping heap entry: %d %d %d\n" n x p; *)
         ignore(Heap.pop_exn heap);
         Heap.add heap (Comp ((x + 2*p), p));
         (* Printf.printf "updatec: %d %d %d %s\n" n x p (((List.to_string l) Int.to_string_hum)); *)
         acc (if x = n then p :: l else l)
      | _ ->
         (* Printf.printf "update: %d %s\n" n (((List.to_string l) Int.to_string_hum)); *)
         l
    in
    acc []

  let rec is_prime heap n =
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
        ignore(Hashtbl.add table x (Calculated (Int.Map.of_alist_exn [x, 1])));
        Prime x
      end
    else
      begin
        let l = update_heap heap x in
        ignore(Hashtbl.add table x (Pending l));
        Composite x
      end

  let factors (_, table) n =
    let incr imap key = match Map.find imap key with
      | None -> Map.add imap ~key:key ~data:1
      | Some v -> (Map.add imap ~key:key ~data:(v + 1))
    in let product = function
         | [] -> 1
         | x :: [] -> x
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
end


let odd_seq upto =
  let t = Sieve_Heap.create upto in
  let (==>) = Lazy_sequence.(==>) in
  let rec f i =
      (* Printf.printf "i: %d\n" i; *)
      if i > upto then
        Lazy_sequence.empty
      else
        Sieve_Heap.next t i ==> fun () -> f (i + 2)
  in
  f 3

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
  
let count_primes ?(s_fun=odd_seq) upto =
  let s = s_fun upto in
  let f count = function
    | Prime p ->
       (* Printf.printf "Heap Found prime: %d\n" p;   *)
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

let string_of_int_pair = function
  | (f, c)  -> Printf.sprintf "%d,%d" f c
                                             
let string_of_factors = function
  | Unique_Odds odds -> Printf.sprintf "Unique_Odds: %s" ((List.to_string odds) Int.to_string)
  | All fs -> Printf.sprintf "All: %s" ((List.to_string fs) string_of_int_pair)

let string_of_natural = function
  | Zero -> "Zero"
  | One -> "One"
  | Prime p -> Printf.sprintf "Prime: %d" p 
  | Composite c ->
     Printf.sprintf "Composite: %d" c 
  | Composite_Factors (c, fs) ->
     Printf.sprintf "Composite: %d %s" c (string_of_factors fs)
  | Composite_Delayed_Factors (c, thunk) ->
     Printf.sprintf "Composite: %d %s" c @@ string_of_factors @@ thunk ()
     

let run () = run_heap 20

let run () =
  let s = factors_seq 50 in
  Lazy_sequence.iteri s ~f:(fun i nat -> Printf.printf "%d %s\n" i (string_of_natural nat))

let run () = bench [1000; 10_000; 100_000; 1_000_000; 10_000_000] 
                                         


