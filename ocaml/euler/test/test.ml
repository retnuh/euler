
let test () = begin
    assert (Lib.Sieve.count_primes 1000 == 168);
    assert (Lib.Sieve.count_primes 100 == 25)
  end

let _ = test ()
