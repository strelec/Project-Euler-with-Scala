import helpers._

val sieve = Sieve(100000)

val result = sieve.composites().find { i =>
	sieve.primes().takeWhile(_ < i - 1).forall { p =>
		val sq = math.sqrt((i - p) / 2).toInt
		i != p + 2*sq*sq
	}
}

println(result)