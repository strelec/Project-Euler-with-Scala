package helpers

case class Sieve(n: Int) {
	private val size = (n - 3) / 2

	private val sieve =
		collection.mutable.BitSet() ++ (0 to size)

	private val root = (math.sqrt(n).toInt - 3) / 2
	for {
		i <- 0 to root
		if sieve(i)
		p = 2*i + 3
		j <- i*(p+3)+3 to size by p
	} sieve(j) = false

	lazy val primes: IndexedSeq[Int] =
		2 +: sieve.toVector.map(2*_ + 3)

	def primeCount(num: Int): Int = {
		import collection.Searching._

		primes.search(num) match {
			case Found(i) => i + 1
			case InsertionPoint(i) => i
		}
	}
	
	def primesIter() =
		Iterator(2) ++ sieve.iterator.map(2*_ + 3)

	def primesIter(from: Int): Iterator[Int] = {
		val it = sieve.iteratorFrom(((from - 2) / 2).toInt).map(2*_ + 3)
		if (from > 2) it else Iterator(2) ++ it
	}

	def oddCompositesIter(from: Int = 9): Iterator[Int] =
		(((from - 2) / 2).toInt to size).iterator.filterNot(sieve).map(2*_ + 3)



	
	def numberOfDivisors(num: Int, power: Int = 1): Int = {
		var n = num
		var result = 1
		primes.takeWhile( p =>
			n != 1 && p*p <= n
		).foreach { p =>
			var cur = 1
			while(n % p == 0) {
				n /= p
				cur += power
			}
			result *= cur
		}
		if (n != 1)
			result *= power + 1
		result	
	}

	def isPrime(num: Long): Boolean =
		if (num <= 2)
			num == 2
		else if (num <= n)
			num%2 == 1 && sieve(((num - 3) / 2).toInt)
		else if (num <= n.toLong*n) {
			val root = math.sqrt(num).toInt
			primes.takeWhile(_ <= root).forall(num % _ != 0)
		} else throw new Exception(s"Cannot check $num for primality in sieve of size $n.")
}

object Comb {
	def partitions(n: Int): Seq[List[Int]] = {
		require(n >= 0)
		def aux(n: Int, max: Int): Seq[List[Int]] = n match {
			case 0 => Seq(Nil)
			case n => for {
				i <- math.min(max, n) to 1 by -1
				prev <- aux(n-i, i)
			} yield i :: prev
		}
		aux(n, n)
	}

	def compositions(n: Int): Seq[List[Int]] = n match {
		case 0 => Seq(Nil)
		case 1 => Seq(List(1))
		case n =>
			val prev = compositions(n - 1)
			prev.map { case h :: t =>
				h + 1 :: t
			} ++ prev.map(1 :: _)
	}

	def subsetSums(l: List[(Int, Int)], sum: Int): Seq[List[Int]] = l match {
		case Nil =>
			if (sum == 0) Seq(Nil) else Seq()
		case List((n, k)) =>
			val i = sum / n
			if (i <= k && i * n == sum) Seq(List(i)) else Seq()
		case (n, k) :: tail =>
			val limit =	if (n == 0) k else math.min(k, sum / n)
			for {
				i <- 0 to limit
				rest <- subsetSums(tail, sum - i*n)
			} yield i :: rest
	}

	// TODO: Optimize
	def fixedSubsetSums(l: List[(Int, Int)], sum: Int, places: Int) =
		subsetSums(l, sum).filter(_.sum == places)

	def cartesian[T](s: List[Seq[T]]): Seq[List[T]] = s match {
		case Nil => Seq(Nil)
		case h :: t => for {
			prev <- cartesian(t)
			el <- h
		} yield el :: prev
	}

	def cartesian[T](s: Seq[T], n: Int): Seq[List[T]] = n match {
		case 0 => Seq(Nil)
		case _ => for {
			prev <- cartesian(s, n-1)
			el <- s
		} yield el :: prev
	}
}

object Number {
	def sum(num: Int): Int = {
		var n = num
		var result = 0
		while(n != 0) {
			result += n % 10
			n /= 10
		}
		result		
	}
	
	def product(num: Int): Int = {
		var n = num
		var result = 1
		while(n != 0) {
			result *= n % 10
			n /= 10
		}
		result		
	}
	
	def count(num: Int): Int = {
		var n = num
		var result = 0
		while(n != 0) {
			result += 1
			n /= 10
		}
		result	
	}

	def reverse(num: Int): Int = {
		var n = num
		var result = 0
		while(n != 0) {
			result *= 10
			result += n % 10
			n /= 10
		}
		result
	}
	
	def isPandigital(num: Int): Boolean = {
		var n = num
		var mask = 1022
		while(n != 0) {
			val d = 1 << (n % 10)
			n /= 10
			if ((d & mask) == 0)
				return false
			mask &= ~d
		}
		mask == 0
	}
	
	def noDuplicates(num: Int): Boolean = {
		var n = num
		var mask = 1022
		while(n != 0) {
			val d = 1 << (n % 10)
			n /= 10
			if ((d & mask) == 0)
				return false
			mask &= ~d
		}
		true
	}
	
	def powers(a: Int) =
		Iterator.iterate(a.toLong)(_ * a)

	def digits(num: Int): List[Int] = {
		var n = num
		var result = List.empty[Int]
		while(n != 0) {
			result ::= n % 10
			n /= 10
		}
		result
	}
}

case class Rational(n: Long, d: Long = 1) extends Ordered[Rational] {

	override def toString =
		if (d == 1) s"$n" else s"$n/$d"
	
	private def create(a: Long, b: Long) =
		if (a == 0) Rational(0, 1) else {
			val gcd = Helpers.gcd(a, b)
			Rational(a/gcd, b/gcd)
		}

	def simplify =
		if (d < 0) create(-n, -d) else create(n, d)

	def compare(that: Rational) =
		n*that.d compare that.n*d

	def isInteger = d == 1

	def isNatural =
		d == 1 && n > 0

	def toLong =  n / d



	def inv = Rational(d, n)
	
	def unary_- = Rational(-d, n)

	def +(that: Rational) =
		create(n*that.d + that.n*d, d*that.d)

	def -(that: Rational) =
		create(n*that.d - that.n*d, d*that.d)

	def *(that: Rational) =
		create(n*that.n, d*that.d)

	def /(that: Rational) =
		create(n*that.d, d*that.n)
}

object Helpers {
	def binoms(n: Int) = {
		def aux(r: Int, acc: BigInt): Stream[BigInt] =
			if (r > n)
				Stream(1)
			else
				acc #:: aux(r+1, acc*(n+1-r)/r)

		aux(1, 1)
	}

	type DoubleRange = scala.collection.immutable.NumericRange[Double]

	def minimize(r: DoubleRange, f: Double => Double) = {
		val gr = (math.sqrt(5) + 1) / 2

		var a = r.start
		var b = r.end
		var c = b - (b-a) / gr
		var d = a + (b-a) / gr

		while (math.abs(c - d) > r.step) {
			if (f(c) < f(d)) b = d else a = c
			c = b - (b-a) / gr
			d = a + (b-a) / gr
		}

		(b + a) / 2
	}

	def findZero(r: DoubleRange, f: Double => Double) = {
		var lo = r.start
		var hi = r.end
		while(hi - lo > r.step) {
			val mid = (hi + lo) / 2
			if (f(mid) * f(lo) > 0) lo = mid else hi = mid
		}
		(hi + lo) / 2
	}

	def gcd(a: Int, b: Int): Int =
		if (b == 0) a else gcd(b, a%b)

	def gcd(a: Long, b: Long): Long =
		if (b == 0) a else gcd(b, a%b)
	
	def gcd(a: BigInt, b: BigInt): BigInt =
		if (b == 0) a else gcd(b, a%b)
}

object SumOfTotients {
	var memo = Map(BigInt(1) -> BigInt(0))
	
	def upTo(n: BigInt): BigInt = memo.getOrElse(n, {
		var sum = n * (n-1) / 2
		var m = BigInt(2)
		while (true) {
			val m2 = n / (n / m)
			if (m2 >= n) {
				val result = sum - (n - m + 1) * upTo(n / m)
				memo += n -> result
				return result
			}
			sum -= (m2 - m + 1) * upTo(n / m)
			m = m2 + 1
		}
		-1
	})
}

class DisjointSets(n: Int) {
	val s = Array.fill(n)(-1)
	
	def union(a: Int, b: Int) {
		if (s(b) < s(a))
			s(a) = b
		else {
			if (s(a) == s(b))
				s(a) -= 1
			s(b) = a
		}
	}
	
	def find(a: Int): Int =
		if (s(a) < 0) a else {
			s(a) = find(s(a))
			s(a)
		}
}

case class PythTriple(a: Int, b: Int, c: Int, k: Int = 1) extends Ordered[PythTriple] {
	def mi = if (a < b) k*a else k*b
	def ma = if (a < b) k*b else k*a
	def compare(that: PythTriple) = that.mi compare mi
	
	override def toString =
		(if (k == 1) (mi,ma,c) else (mi, ma, k*c, k)).toString
	
	def next = {
		val t = 2*(a + b + c)
		val (x, y, z) = (t - a, t - b, t + c)
		val kp1 = this.copy(k = k+1)
		if (k != 1) Seq(kp1) else Seq(
			kp1,
			PythTriple(x - 4*b, y - 2*b, z - 4*b),
			PythTriple(x, y, z),
			PythTriple(x - 2*a, y - 4*a, z - 4*a)
		)
	}
}

object PythTriple {
	def inOrder = {
		val q = collection.mutable.PriorityQueue( PythTriple(3,4,5) )
		Iterator.continually {
			val min = q.dequeue
			min.next.foreach(q.enqueue(_))
			min
		}
	}
}
