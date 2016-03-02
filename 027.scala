val sieve = helpers.Sieve(20000)
val	primes = sieve.primes

val result = for {
	b <- primes.takeWhile(_ < 1000)
	a <- primes.map(_ - b - 1).takeWhile(_ < 1000)
	c = Iterator.from(0).map( n =>
		(n*n + a*n + b).toLong
	).takeWhile(sieve.isPrime).size
} yield (c, a * b)

println(result.max._2)
