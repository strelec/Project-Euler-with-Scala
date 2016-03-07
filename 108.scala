val N = 1000
val sieve = helpers.Sieve(1000)

val result = Iterator.from(2).find(
	sieve.numberOfDivisors(_, 2)/2 + 1 > N
)
println(result.head)
