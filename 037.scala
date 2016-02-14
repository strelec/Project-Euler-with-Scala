import helpers._

val sieve = Sieve(1000000)

val result = sieve.primes.dropWhile(_ < 10).filter { p =>
	val str = p.toString
	val subs = str.inits.slice(1, str.size) ++ str.tails.slice(1, str.size)
	subs.forall(x => sieve.isPrime(x.toLong))
}

println(result.sum)