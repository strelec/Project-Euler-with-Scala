val N = 1000000000
val sieve = helpers.Sieve(math.sqrt(N).toInt + 1)

case class A(n: Long, max: Int) extends Ordered[A] {
	def compare(that: A) = n compare that.n
	
	def advance = (0 to max+1).map( i =>
		A(n*sieve.primes(i), math.max(max, i))
	)
}

val admissible = {
	val q = collection.mutable.SortedSet(A(2,0))
	var result = List.empty[Int]
	while (q.min.n < N) {
		val el = q.min
		q -= el
		el.advance.foreach { a =>
			q += a
		}
		result ::= el.n.toInt
	}
	result
}

val result = admissible.map { n =>
	var i = 2
	while (!sieve.isPrime(n+i)) i += 1
	i
}

println(result.distinct.sum)
