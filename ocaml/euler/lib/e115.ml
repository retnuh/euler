open Core

let min_blocks = ref 3

let rec print_indent = function
  | 0 -> ()
  | n -> print_string "\t"; print_indent (n-1)

(*
let memo_rec f =
  let m = ref [] in
  let rec g x =
    try
      List.assoc x !m
    with
    Not_found ->
      let y = f g x in
        m := (x, y) :: !m ;
        y
  in
  g

let fib_rec_memo_trivial n =  
  let table = Hashtbl.Poly.create () in
  let rec fib_rec_memo x = 
    match Hashtbl.find table x with
    | Some y -> y
    | None ->
      let y = fib_rec_memo (x-1) + fib_rec_memo (x-2) in
      Hashtbl.add_exn table ~key:x ~data:y;
      y
  in
  fib_rec_memo
  *)

let count_one ca blocks total_space =
  (* print_indent indent; *)
  (* Printf.printf "count_one: %d %d\n" blocks total_space;   *)
  let r = if blocks + 1 >= total_space then
      1
    else if blocks + 1 = total_space then
      1
    else
      ca (total_space - blocks -1)
  in
  (* print_indent indent; *)
  (* Printf.printf "count_one: %d %d -> %d\n" blocks total_space r; *)
  r

let count_all_base self total_space =
  (* print_indent indent; *)
  (* Printf.printf "count_all: %d\n" total_space;  *)
  let r = if total_space < !min_blocks then
      1
    else
      let rec loop_all total = function
        | -1 -> total 
        | n -> loop_all (total + (count_one self (!min_blocks + n) total_space)) (n -1)
      in loop_all (self (total_space - 1)) (total_space - !min_blocks)
  in
  (* print_indent indent; *)
  (* Printf.printf "count_all result: %d -> %d\n" total_space r;  *)
  r

let memo_ref f =
  let table = Hashtbl.Poly.create () in
  let rec g x = match Hashtbl.find table x with
    | Some y -> y
    | None ->
      let y = f g x in
      Hashtbl.add_exn table ~key:x ~data:y;
      y
  in
  g



(*
      let run () = print_int (count_one 0 3 7); print_newline (); print_newline ()

 *)

let count_all = memo_ref count_all_base

let e115 () = 
  min_blocks := 50;
  let total_blocks = ref 100 in
  let count = ref @@ count_all !total_blocks in begin
    while !count < 1000000 do
      incr total_blocks;
      count := count_all !total_blocks
    done;
    Printf.printf "%d -> %d \n\n" !total_blocks !count
  end

(* let size = if (Array.length Sys.argv > 1) then
    int_of_string Sys.argv.(1)
   else
    50 *)

(* let run () = Printf.printf "%d\n\n" @@ count_all @@ int_of_string Sys.argv.(1)  *)

