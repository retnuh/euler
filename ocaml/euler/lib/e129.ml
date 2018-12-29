
let z10 = Z.(one * Z.of_int 10)
let z9 = Z.(one * Z.of_int 9)

let repunit k = 
  Z.(z10 ** k / z9)

let%test _ = repunit 6 = Z.of_int 111111

