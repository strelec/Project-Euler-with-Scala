val N = 100000000
val sieve = helpers.Sieve(N)

val result = for {
	 p <- sieve.primesIter
	 n = p - 1
	 divisors = sieve.divisorsOf(n)
	 if divisors.size % 2 == 0
	 if divisors.sorted.take(divisors.size/2).forall( d =>
	 	sieve.isPrime(d + n/d)
	 )
} yield n.toLong

println(1 + result.sum)
