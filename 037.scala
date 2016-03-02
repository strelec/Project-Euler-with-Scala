val sieve = helpers.Sieve(800000)

val result = sieve.primesIter(10).filter { p =>
	val str = p.toString
	val subs = str.inits.slice(1, str.size) ++ str.tails.slice(1, str.size)
	subs.forall(x => sieve.isPrime(x.toLong))
}.take(11).toList

require(result.size == 11, "The sieve is not big enough.")
println(result.sum)
