val sieve = helpers.Sieve(200)

val result = "9876543210".permutations.filter( perm =>
	(1 to 9).exists( i =>
		if (perm(i) == '0') false else {
			val a = perm.take(i).toInt
			val b = perm.drop(i).toInt
			
			sieve.divisorsOf(helpers.Helpers.gcd(a, b)).exists { c =>
				val s = c.toString + (a/c).toString + (b/c).toString
				s.size == 10 && s.distinct.size == 10
			}
		}
	)
)

println(result.next)
