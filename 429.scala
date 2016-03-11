val N = 100000000
val MOD = 1000000009

val primes = helpers.Sieve(N).primesIter
val result = primes.map { p =>
	val m = helpers.Number.powers(p).takeWhile(_ <= N).map(N/_).sum
	BigInt(p).modPow((m+m) % MOD, MOD).toLong + 1
}.reduce(_ * _ % MOD)

println(result)
