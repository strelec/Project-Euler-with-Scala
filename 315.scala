val primes = helpers.Sieve(2*1000*1000).primesIter(1000*1000).map(
	_.toString.map(_.asDigit).reverse
).toVector

val digits = Vector(
	Vector(1,1, 1,0,1, 1,1),
	Vector(0,0, 0,0,0, 1,1),
	Vector(0,1, 1,1,1, 1,0),
	Vector(0,0, 1,1,1, 1,1),
	Vector(1,0, 0,1,0, 1,1),
	Vector(1,0, 1,1,1, 0,1),
	Vector(1,1, 1,1,1, 0,1),
	Vector(1,0, 1,0,0, 1,1),
	Vector(1,1, 1,1,1, 1,1),
	Vector(1,0, 1,1,1, 1,1),
	
	Vector(0,0, 0,0,0, 0,0)
)

val sam = {
	val counts = digits.map(
		_.count(_ == 1)
	)

	2 * primes.flatten.map(counts).sum
}
val max = {
	val diff = Vector.tabulate(11, 11) { case (i, j) =>
		(0 until 7).count( c => digits(i)(c) != digits(j)(c))
	}
	
	primes.sliding(2).map { case Seq(a, b) =>
		a.zipAll(b, 10, 10).map { case (ad, bd) =>
			diff(ad)(bd)
		}.sum
	}.sum
}

println(sam - max)
