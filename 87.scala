import helpers._

val n = 50000000
val sieve = Sieve(math.sqrt(n).toInt)

val al = math.pow(n, 0.25).toInt

val result = for {
	a <- sieve.primes.takeWhile(p => p <= al)
	at = a*a*a*a

	bl = math.cbrt(n - at).toInt
	b <- sieve.primes.takeWhile(p => p <= bl)
	bt = b*b*b

	cl = math.sqrt(n - at - bt).toInt
	c <- sieve.primes.takeWhile(p => p <= cl)
	ct = c*c
} yield at+bt+ct

println(result.distinct.size)