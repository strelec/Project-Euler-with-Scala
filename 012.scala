val N = 500
val sieve = helpers.Sieve(10000)

val result = for {
	i <- Iterator.from(1)
	n = i * (i + 1) / 2
	if N < sieve.numberOfDivisors(n)
} yield n

println(result.next)
