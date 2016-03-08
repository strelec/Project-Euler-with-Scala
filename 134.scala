val N = 1000000
val sieve = helpers.Sieve(N+200)
val p10 = helpers.Number.powers(10).map(BigInt(_)).take(15).toVector

val result = for {
	Seq(a, b) <- sieve.primesIter(5).sliding(2)
	if a <= N
	y = p10(helpers.Number.count(a) - 1)
	r = y.modInverse(b).toLong * (b - a) % b
} yield y*r + a

println(result.sum)
