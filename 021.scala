val sieve = helpers.Sieve(100)

val result = (2 until 10000).filter { i =>
	val other = (sieve.sumOfDivisors(i) - i).toInt
	i != other && i == sieve.sumOfDivisors(other) - other
}

println(result.sum)
