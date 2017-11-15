val N = 5000 - 1
val MOD = BigInt(10).pow(16).toLong

val primes = helpers.Sieve(N).primes
val memo = Array.fill(primes.sum + 1)(0L)
memo(0) = 1

for {
	p <- primes
	i <- memo.indices.drop(p).reverse
} {
	memo(i) += memo(i - p)
	memo(i) %= MOD
}

val bigPrimes = memo.indices.iterator.filter(BigInt(_).isProbablePrime(8))
val result = bigPrimes.map(memo).reduce( (a, b) => (a + b) % MOD )
println(result)
