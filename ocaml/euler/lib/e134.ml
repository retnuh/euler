open Core_kernel
open Numbers

let rec endswith a b =
  if b = 0 then true else
    let ad, am = divmod a 10 
    and bd, bm = divmod b 10 
    in if am = bm then endswith ad bd else false


let%test _ = endswith 1219 19 = true
let%test _ = endswith 1219 18 = false
let%test _ = endswith 1219 29 = false

let connection p1 p2 = p1+p2

let%test _ = connection 19 23 = 1219