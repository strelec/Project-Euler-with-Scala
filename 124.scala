val N = 100000
val sieve = sieves.Radical(N)

val result = (1 to N).map( n =>
	sieve(n) -> n
).sorted

println(result(10000 - 1)._2)
