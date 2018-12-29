open Core

let one_to_nine = List.init 8 ~f:(fun x -> Z.of_int (x+1))

let sum_same_digits_for_base acc seed =
  List.fold one_to_nine ~init:acc  ~f:(fun acc x -> Z.( (x * seed) + acc))

let sum_upto upto_pow =
  let upto = Z.( (of_int 10) ** upto_pow ) in
  let ten = Z.of_int 10 in
  let rec aux acc seed = 
    if Z.lt seed upto then aux (sum_same_digits_for_base acc seed) Z.( ten * seed + one)
    else acc
  in aux Z.zero (Z.of_int 11)

let shortcut_same_digits seed =
  let c = List.init 9 ~f:(fun x -> (x+1) * seed) 
          (* |> List.map ~f:(fun x -> print_endline @@ string_of_int x; x) *)
          |> List.fold ~init:0 ~f:(+) in
  (* print_endline @@ string_of_int c; *)
  c mod 100000

let shortcut_5_wide_same_digits count =
  let c = shortcut_same_digits 11111 |> ( * ) count in
  (* print_endline @@ string_of_int c; *)
  c mod 100000


let%test _ = shortcut_5_wide_same_digits 1 = (11111 + 22222 + 33333 + 44444 + 55555 + 66666 + 77777 + 88888 + 99999) mod 100000

let shortcut_sum_same_digits_upto_10k = 
  let c = List.fold [11; 111; 1111] ~init:0 ~f:(fun acc seed -> acc + shortcut_same_digits seed) in 
  c mod 100000


let right_rotate_int n = 
  let (rest, ones) = Numbers.divmod n 10
  and digits = int_of_float @@ Float.log10 @@ float_of_int n 
  in let rr = ones * Int.pow 10 digits + rest  
  in 
  (* [rest; ones; digits; rr] |> List.map ~f:string_of_int |> String.concat ~sep:" " |> print_endline; *)
  rr

let%expect_test _ =
  let ans = right_rotate_int 142857 
  in print_endline @@ string_of_int ans;
  [%expect{|
    714285
   |}] 

let%test _ = right_rotate_int 142857 = 714285

let is_exceptional_divisor_of_right_rotation n = 
  let rr = right_rotate_int n in
  rr > n && rr mod n = 0

let%test _ = is_exceptional_divisor_of_right_rotation 142857 = true

let exceptional_divisors_of_right_rotation_in_range from upto =
  List.range from upto |> List.filter ~f:is_exceptional_divisor_of_right_rotation 

let print_known_exceptional n =
  exceptional_divisors_of_right_rotation_in_range 10 n 
  |> List.map ~f:(fun x -> (x, right_rotate_int x, (right_rotate_int x)/ x)) 
  |> List.map ~f:(fun (a,b,c) -> Printf.sprintf "%d,%d,%d" a b c) |> List.iter ~f:print_endline

let run n = print_known_exceptional n

let run_bad _ =
  let ex = exceptional_divisors_of_right_rotation_in_range 10 1000000 |> List.fold ~init:0 ~f:(+) 
  and small = shortcut_sum_same_digits_upto_10k
  and rest = shortcut_5_wide_same_digits 1
  in  ((ex + small + rest)) |> Printf.printf "run_bad %d\n"

(* 
2018-12-28 unsolved - don't know how to find divisors of right rotations non brute force other
than the obvious ones like 111 333 etc.
*)  

(* Hmm should check results up through 10^6 = 98331 *)

let is_divisor_of_right_rotation n = 
  let rr = right_rotate_int n in
  rr >= n && rr mod n = 0

let%test _ = 
  (* print_known_exceptional (Int.pow 10 10); *)
  let all = List.range 11 (Int.pow 10 6) |> List.filter ~f:is_divisor_of_right_rotation in
  let sum = List.fold all ~init:0 ~f:(+) in
  let summod = sum mod 100000 in
  (* all |> List.sexp_of_t Int.sexp_of_t |> Sexp.to_string |> Printf.printf "all: %s\n";  *)
  (* Printf.printf "sum: %d\n" sum;  *)
  summod = 98331