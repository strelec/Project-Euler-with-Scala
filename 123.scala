val N = 10000000000L

val sieve = helpers.Sieve(300000)

val result = for {
	(p, n) <- sieve.primesIter.zip(Iterator.from(1))
	if n % 2 == 1
	if 2L * n * p > N
} yield n

require(result.hasNext, "The sieve is not big enough.")
println(result.next)
