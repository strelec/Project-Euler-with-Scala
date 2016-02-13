import helpers._

val n = 100000000
val sieve = Sieve(n)

val result = sieve.primes.takeWhile(i => i*i <= n).map( p =>
	sieve.primeCount((n / p).toInt) - sieve.primeCount(p-1)
).sum

println(result)