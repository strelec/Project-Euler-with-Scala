val N = 1000000000000L

class HammingNumbers extends Iterator[Long] {
	val q = collection.mutable.SortedSet(1L)
	
	def next = {
		val result = q.head
		q.remove(result)
		q.add(result * 2)
		q.add(result * 3)
		q.add(result * 5)
		result
	}
	
	def hasNext = true
}

class AllPossibleProducts(a: Array[Long], limit: Long) extends Iterator[Long] {
	var q = collection.mutable.Queue(a.zipWithIndex: _*)
	
	def next = {
		val (v, position) = q.dequeue
		var i = position + 1
		if (i < a.size && a(i) <= Long.MaxValue / v) { // product overflow check
			while (i < a.size && v * a(i) <= limit) {
				q.enqueue((v * a(i), i))
				i += 1
			}
		}
		v
	}
	
	def hasNext = q.nonEmpty
}

val smooth = new HammingNumbers().takeWhile(_ <= N).toArray
val primes = smooth.map(_ + 1).filter(
	BigInt(_).isProbablePrime(1000)
).drop(3).takeWhile(_ <= N)

var result = smooth.sum.toLong
new AllPossibleProducts(primes, N).foreach { i =>
	// this loop could be improved using binary search & cummulative sums
	smooth.takeWhile(_ * i <= N).foreach { s =>
		result += s * i
	}
}

result %= 1L << 32
result += 1L << 32
result %= 1L << 32

println(result)
