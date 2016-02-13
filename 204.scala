import helpers.Sieve

val max = 1000000000

def rec(prod: Long, lst: List[Int]): Int = lst match {
	case head :: tail =>
		var next = prod
		var sum = 0
		while (next <= max) {
			sum += rec(next, tail)
			next *= head
		}
		sum
	case _ => 1
}

val primes = Sieve(100).primes.reverse.toList
val result = rec(1, primes)

println(result)
