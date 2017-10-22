package sieves

import scala.reflect.ClassTag

abstract class Sieve[T: ClassTag](n: Int, init: T) {
	var data: Array[T] = _
	val remainder = (0 to n).toArray
	
	def initialize {
		data = Array.fill(n+1)(init)
	}
	initialize
	
	
	def primeAction(p: Int) { compositeAction(p, p, 1) }
	def compositeAction(p: Int, i: Int, power: Int) {}
	
	val m = math.sqrt(n).toInt
	val sieve = Array.fill(m+1)(true)
	
	(2 to m).foreach { i =>
		if (sieve(i)) {
			primeAction(i)
			(i+i to m by i).foreach { j =>
				sieve(j) = false
			}
			(i+i to n by i).foreach { j =>
				var power = 0
				while (remainder(j) % i == 0) {
					remainder(j) /= i
					power += 1
				}
				compositeAction(i, j, power)
			}
		}
	}
	
	(m+1 to n).foreach { i =>
		if (remainder(i) != 1)
			compositeAction(remainder(i), i, 1)
	}
	
	def apply(i: Int) = data(i)
	
	def primes = {
		(1 to m).filter(sieve) ++ (m+1 to n).filter( i => remainder(i) == i )
	}
}




case class DivisorCount(n: Int) extends Sieve[Int](n, 1) {
	override def compositeAction(p: Int, i: Int, power: Int) {
		data(i) *= power + 1
	}
}

case class DivisorSum(n: Int) extends Sieve[Int](n, 1) {
	override def compositeAction(p: Int, i: Int, power: Int) {
		var sum = 0
		var factor = 1
		(0 to power).foreach { _ =>
			sum += factor
			factor *= p
		}
		data(i) *= sum
	}
}

case class Divisors(n: Int) extends Sieve[List[Int]](n, List(1)) {
	override def compositeAction(p: Int, i: Int, power: Int) {
		var result = data(i)
		var factor = p
		(1 to power).foreach { _ =>
			result :::= data(i).map(_ * factor)
			factor *= p
		}
		data(i) = result
	}
}

case class FactorCount(n: Int) extends Sieve[Int](n, 0) {
	override def compositeAction(p: Int, i: Int, power: Int) {
		data(i) += 1
	}
}

case class FactorSum(n: Int) extends Sieve[Int](n, 0) {
	override def compositeAction(p: Int, i: Int, power: Int) {
		data(i) += p
	}
}

case class AllFactors(n: Int) extends Sieve[List[Int]](n, Nil) {
	override def compositeAction(p: Int, i: Int, power: Int) {
		data(i) :::= List.fill(power)(p)
	}	
}

case class Factors(n: Int) extends Sieve[List[Int]](n, Nil) {
	override def compositeAction(p: Int, i: Int, power: Int) {
		data(i) ::= p
	}
}

case class Radical(n: Int) extends Sieve[Int](n, 1) {
	override def compositeAction(p: Int, i: Int, power: Int) {
		data(i) *= p
	}
}

case class Mobius(n: Int) extends Sieve[Int](n, 1) {
	override def compositeAction(p: Int, i: Int, power: Int) {
		if (i % (p * p) == 0) {
			data(i) = 0
		} else {
			data(i) *= -1
		}
	}
}

case class Totients(n: Int) extends Sieve[Int](n, 1) {
	override def initialize {
		data = (0 to n).toArray
	}
	
	override def compositeAction(p: Int, i: Int, power: Int) {
		data(i) = data(i) / p * (p-1)
	}	
}
