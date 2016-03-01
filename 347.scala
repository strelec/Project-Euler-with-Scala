val N = 10000000

import helpers.Number.powers

def best(a: Int, b: Int) = (for {
	bp <- powers(b).takeWhile(_ < N)
	ap <- powers(a).takeWhile(_ * bp <= N)
} yield ap * bp).max

val sieve = helpers.Sieve(N/2)

val result = for {
	a <- sieve.primesIter.takeWhile(p => p*p < N)
	b <- sieve.primesIter(a+1).takeWhile(_ * a <= N)
} yield best(a, b)

println(result.sum)
