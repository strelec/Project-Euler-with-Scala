val N = 100000
val sieve = helpers.Sieve(1000)

val result = (1 to N).map( n =>
	sieve.radicalOf(n) -> n
).sorted

println(result(10000 - 1)._2)
