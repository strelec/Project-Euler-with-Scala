val N = 1L << 50
val M = 1 << 25
val sieve = helpers.Sieve(M)

val result = (1 to M).map { d =>
	N / (d.toLong*d) * sieve.mobius(d)
}.sum

println(result)

