val sieve = helpers.Sieve(1000000)
val primes = sieve.primes

class A(a: Int, cub: Long, b: Int) extends Ordered[A] {
	val v = cub * primes(b) * primes(b)
	
	def next =
		if (b + 1 == a) new A(a, cub, b + 2)
		else            new A(a, cub, b + 1)
		
	def compare(that: A): Int = that.v.compareTo(v)
}

val iter = {
	val q = new collection.mutable.PriorityQueue[A]()
	q += new A(0, 8, 1)
	q ++= (1 to 1000).map { i =>
		val p = primes(i).toLong
		new A(i, p*p*p, 0)
	}

	Iterator.continually {
		val el = q.dequeue
		q += el.next
		el.v
	}
}

def is200(a: Long) = {
	var b = a
	while (b != 0 && b % 1000 != 200)
		b /= 10
	b != 0
}

def isPrimeProof(a: Long): Boolean = {
	var i = 1L
	while(a / i != 0) {
		val digit = ((a / i) % 10).toInt
		
		(1 to digit).foreach { d =>
			if (sieve.isPrime(a - d*i)) return false
		}
		(1 to 9-digit).foreach { d =>
			if (sieve.isPrime(a + d*i)) return false
		}
		
		i *= 10
	}
	true
}

val result = iter.filter(is200).filter(isPrimeProof).take(200).toVector

println(result.last)
