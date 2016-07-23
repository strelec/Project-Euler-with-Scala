val N = 120000 - 1

val factors = sieves.Factors(N)
val radicals = sieves.Radical(N)

val result = for {
	c <- 0 to N
	rem = c / radicals(c)
	if rem > 1
	
	a <- 1 until c/2
	if radicals(a) < rem
	b = c - a
	if radicals(a) * radicals(b) < rem
	if (factors(a) intersect factors(b)).isEmpty
} yield c

println(result.sum)
