val N = 1L << 50
val M = 1 << 25
val sieve = sieves.Mobius(M)

val result = (1 to M).map { d =>
	N / (d.toLong*d) * sieve(d)
}.sum

println(result)

