val N = 1000000
val sieve = helpers.Sieve(N/4)

val result = (2 to N/4).map(
	sieve.numberOfDivisors(_)/2
)
println(result.count(_ <= 10))
