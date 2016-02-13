val factoring = Vector.tabulate(13000)(
	helpers.NativeFactoring(_).divisors.tail
)

val result = "9876543210".permutations.filter( perm =>
	(1 to 9).exists( i =>
		if (perm(i) == '0') false else {
			val a = perm.take(i).toInt
			val b = perm.drop(i).toInt
			
			factoring(helpers.Helpers.gcd(a, b)).exists { c =>
				val s = c.toString + (a/c).toString + (b/c).toString
				s.size == 10 && s.distinct.size == 10
			}
		}
	)
).next

println(result)
