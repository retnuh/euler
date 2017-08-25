
open Core
       
type factor = int * int

type factors =
  | Unique_Odds of int list
  | All of factor list
                                
type natural =
  | Zero
  | One
  | Prime of int
  | Composite of int
  | Composite_Factors of int * factors
  | Composite_Delayed_Factors of int * (unit -> factors)
                                      
let is_even x = Int.bit_and x 0x1 = 0

let is_odd x =  not (is_even x)

