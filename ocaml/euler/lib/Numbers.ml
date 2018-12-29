
open Core

type factor = int * int

type factors =
  | Unique_Odds of int list
  | Primes of int list
  | All of factor list

type natural =
  | Zero
  | One
  | Prime of int
  | Composite of int
  | Composite_Factors of int * factors
  | Composite_Delayed_Factors of int * (unit -> factors)

let string_of_int_pair = function
  | (f, c)  -> Printf.sprintf "%d,%d" f c

let string_of_factors = function
  | Unique_Odds odds -> Printf.sprintf "Unique_Odds: %s" (List.to_string odds ~f:Int.to_string)
  | All fs -> Printf.sprintf "All: %s" (List.to_string fs ~f:string_of_int_pair)
  | Primes ps -> Printf.sprintf "Primes: %s" (List.to_string ps ~f:Int.to_string)

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



let is_even x = Int.bit_and x 0x1 = 0

let is_odd x =  Int.bit_and x 0x1 = 1

let divmod x m =
  if (x < m) then
    0, x
  else
    let d = x/m in
    let r = x - (m*d)
    in d,r

let factorial n =  
  if n < 0 then failwith "Cannot do factorial of negative number" else
    let rec aux acc = function
      | 0 | 1 -> acc
      | x -> aux (acc*x) (x-1)
    in aux 1 n

let%test _ = factorial 3 = 6
let%test _ = factorial 5 = 120

let choose n k = (factorial n)/(factorial k * factorial (n-k))

let%test _ = choose 5 2 = 10

let cdiv a b = if is_even a then a / b else a / b + 1

let%test _ = cdiv 5 3 = 2
let%test _ = cdiv 8 4 = 2

let chalf a = Int.(if is_even a then (a lsr 1) else (a lsr 1) + 1) 

let%test _ = chalf 5 = 3
let%test _ = chalf 8 = 4
