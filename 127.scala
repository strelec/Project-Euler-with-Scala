val N = 120000 - 1

val factors = (0 to N).map(helpers.NativeFactoring(_).factor.keySet)
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
