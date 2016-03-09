val N = 10
val sieve = helpers.Sieve(100000)

val p10 = 1L +: helpers.Number.powers(10).take(N-1).toVector
val base = p10.sum

val result = (0 to 9).map { d =>
	val others = (0 to 9).diff(Seq(d))
	Iterator.range(1, N-1).map( i =>
		for {
			comb <- helpers.Comb.cartesian(others, i)
			pos <- (0 until N).combinations(i)
			diff = (comb, pos).zipped.map( (digit, place) =>
				(digit - d) * p10(place)
			).sum
			candidate = d*base + diff
			if candidate >= p10.last
			if sieve.isPrime(candidate)
		} yield candidate
	).filter(_.nonEmpty).next
}

println(result.flatten.sum)
