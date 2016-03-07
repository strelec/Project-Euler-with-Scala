val N = 4000000

val primes = helpers.Sieve(100).primes
val q = collection.mutable.PriorityQueue( (-2L, 3, 0, 1) )

val it = Iterator.continually {
	val (n, d, i, a) = q.dequeue
	q.enqueue((
		n * primes(i),
		d / (2*a+1) * (2*a+3),
		i,
		a + 1
	))
	q.enqueue((
		n * primes(i+1),
		d * 3,
		i + 1,
		1
	))
	(-n, d)
}

val result = it.find(_._2 > 2*N + 2).get
println(result._1)
