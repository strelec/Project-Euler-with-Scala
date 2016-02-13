import helpers._

val sieve = Sieve(999999)

val result = sieve.primes().filter { p =>
	val str = p.toString
	(for {
		i <- (1 to str.size-1).toStream
		(a, b) = str.splitAt(i)
		newp = (b + a).toInt
	} yield newp).forall(sieve.isPrime)
}

println(result.size)