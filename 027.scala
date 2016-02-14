import helpers._

val sieve = Sieve(20000)
val stream = sieve.primes()

val (c, a, b) = (for {
	b <- stream.takeWhile(_ < 1000)
	a <- stream.map(_ - b - 1).takeWhile(_ < 1000)
} yield {
	def eq(n: Int) = n*n + a*n + b
	val common = Stream.from(0).map(eq).takeWhile(sieve.isPrime).size
	(common, a, b)
}).max

println(a*b)