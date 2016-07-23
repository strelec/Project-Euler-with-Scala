val N = 10000 - 1

val sieve = sieves.DivisorSum(3 * N)

val result = (2 to N).filter { i =>
	val other = sieve(i) - i
	i != other && i == sieve(other) - other
}

println(result.sum)
