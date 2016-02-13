val N = 40000000 - 1
val sieve = helpers.Sieve(N)

val lengths = Array.fill(N+1)(1)
(2 to N).foreach { i =>
	lengths(i) = 1 + lengths(sieve.totients(i))
}

val result = sieve.primes.filter(lengths(_) == 25).map(_.toLong).sum
println(result)
