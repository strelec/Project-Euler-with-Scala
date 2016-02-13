import helpers._

val sieve = Sieve(100)

@inline
def t(a: Long, b: Long, mod: Int) =
	a * b % mod

@inline
def factmod(n: Int, mod: Int): Long =
	(2L to n).foldLeft(0L)(_ * _ % mod)



val result = sieve.primesIter().drop(2).map { p =>
	val q = p.toLong - 1
	//factmod(p-5, p) * (q * q % p * (q-4) + 1) % p * (q - 3) % p
	t(t(t(t(q, q, p), q-2, p) + 1, q-3, p) + 1, factmod(p-5, p), p)
}.sum

println(result)
