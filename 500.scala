val N = 500500
val MOD = 500500507

val sieve = helpers.Sieve(8000000)
val q = collection.mutable.PriorityQueue(sieve.primes.map(-_.toLong): _*)

var result = 1L
(1 to N).foreach { _ =>
	val top = q.dequeue
	result *= -top
	result %= MOD
	q.enqueue(- top * top)
}

println(result)
