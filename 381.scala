val N = 100000000
val sieve = helpers.Sieve(N)

println(sieve.primesIter(5).map( p =>
	(p.toLong*p - 1)/8 * 3 % p
).sum)
