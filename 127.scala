val N = 120000 - 1

val sieve = helpers.Sieve(1000)
val factors = (0 to N).map(sieve.factorsOf(_).toSet)
val radicals = factors.map(_.product)

val result = for {
	c <- 0 to N
	rem = c / radicals(c)
	if rem > 1
	
	a <- 1 until c/2
	if radicals(a) < rem
	b = c - a
	if radicals(a) * radicals(b) < rem
	if (factors(a) & factors(b)).isEmpty
} yield c

println(result.sum)
