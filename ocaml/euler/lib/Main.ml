
let time f =
  let start = Unix.gettimeofday ()
  in f (); 
  let stop = Unix.gettimeofday ()
  in Printf.printf "Execution time: %fs\n%!" (stop -. start)

module Mod = E204

let run () = 
  let n= if (Array.length Sys.argv > 1) then
      int_of_string Sys.argv.(1)
    else
      Mod.run_default
  in time (fun () -> Printf.printf "Answer: %d\n" @@ Mod.run n)

(* let run () = E407.e407_bench [1000; 10_000; 100_000] 

   let run () = time (fun () -> E407.run size) *)


