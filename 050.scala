import helpers.Sieve

val sieve = Sieve(1000000)

val cumul = sieve.primes.map {
	var s = 0
	d => {s += d; s}
}

val results = for {
	length <- (550 to 1 by -1).toStream
	start <- 0 until cumul.size - length

	diff = cumul(start + length) - cumul(start)
	if diff < 1000000
	if sieve.isPrime(diff)
} yield diff

println(results.head)