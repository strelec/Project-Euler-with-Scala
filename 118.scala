val sieve = helpers.Sieve(10000000)

// 1 + 8 sets
val trivial = (for {
	p <- Seq("2","5","7").iterator
	perm <- ("123456789" diff p).permutations
	if sieve.isPrime(perm.toInt)
} yield perm).size

val primes =
	sieve.primes.filter(helpers.Number.noDuplicates).groupBy(helpers.Number.count)
	
def count(rem: List[Int]) = {
	def aux(set: String, rem: List[Int], max: Int): Int = rem match {
		case Nil => if (set == "") 1 else 0
		case h :: t =>
			primes(h).filter(_ < max).map( p =>
				aux(set diff p.toString, t, p)
			).sum
	}
	aux("123456789", rem, Int.MaxValue)
}

val result = helpers.Comb.partitions(9).drop(2).map(count)
println(result.sum + trivial)
