val sieve = helpers.Sieve(10000)

val result = for {
	n <- (8 to 1 by -1).iterator
	i <- (n to 1 by -1).permutations
	p = i.mkString.toLong
	if sieve.isPrime(p)
} yield p

println(result.next)
