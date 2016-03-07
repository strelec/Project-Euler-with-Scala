val N = 1000000000

val sieve = helpers.Sieve(math.sqrt(N).toInt + 1)
val q = collection.mutable.PriorityQueue( (-2L, 0) )

val admissible = Iterator.continually {
	val (n, i) = q.dequeue
	q.enqueue((n * sieve.primes(i), i))
	q.enqueue((n * sieve.primes(i+1), i+1))
	-n
}

val result = admissible.takeWhile(_ < N).map { n =>
	var i = 2
	while (!sieve.isPrime(n+i)) i += 1
	i
}.toSet

println(result.sum)
