val biggest = 1100

val p = helpers.Sieve(85000000).primes

var current = {
	val check = p.toSet
	for {
		i <- 0 to biggest
		j <- i+1 to biggest
		if check(("" + p(i) + p(j)).toInt)
		if check(("" + p(j) + p(i)).toInt)
	} yield List(j, i)
}

val friends = current.map {
	case List(j, i) => (i, j)
}.toSet

(3 to 5).foreach { _ =>
	current = for {
		group <- current
		candidate <- group.head+1 to biggest
		if group.forall(friends(_, candidate))
	} yield candidate :: group
}

require(current.size == 1)
val result = current.head.map(p) 
println(result.sum)
