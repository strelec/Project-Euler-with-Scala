def A(n: Int) = {
	var cur = 0
	var i = 0
	do {
		cur = (cur * 10 + 1) % n
		i += 1
	} while (cur != 0)
	i
}

val isPrime = helpers.Sieve(30000).primes.toSet
val candidates = Iterator.from(2).filterNot(n =>
	n % 2 == 0 || n % 5 == 0 || isPrime(n)
)

val result = candidates.filter(n => (n - 1) % A(n) == 0).take(25)
println(result.sum)
