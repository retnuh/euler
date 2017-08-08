

let min_blocks = 3

let rec print_indent = function
  | 0 -> ()
  | n -> print_string "\t"; print_indent (n-1)
                                             
let rec count_all indent total_space count_one =
  (* print_indent indent; *)
  (* Printf.printf "count_all: %d\n" total_space;  *)
  let r = if total_space < min_blocks then
            1
          else
            let rec loop_all total = function
              | -1 -> total 
              | n -> loop_all (total + (count_one (indent + 1) (min_blocks + n) total_space)) (n -1)
            in loop_all (count_all (indent + 1) (total_space - 1) count_one) (total_space - min_blocks)
  in
  (* print_indent indent; *)
  (* Printf.printf "count_all result: %d -> %d\n" total_space r;  *)
  r

let rec count_one indent blocks total_space =
  (* print_indent indent; *)
  (* Printf.printf "count_one: %d %d\n" blocks total_space;   *)
  let r = if blocks + 1 >= total_space then
            1
          else if blocks + 1 = total_space then
            1
          else
            count_all (indent + 1) (total_space - blocks -1) count_one
  in
  (* print_indent indent; *)
  (* Printf.printf "count_one: %d %d -> %d\n" blocks total_space r; *)
  r
    
  
(*
      let run () = print_int (count_one 0 3 7); print_newline (); print_newline ()

 *)
let run () = print_int (count_all 0 30 count_one); print_newline (); print_newline ()

let add2 x = x + 2
