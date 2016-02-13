val N = 12000

val result = for {
	d <- 1 to N
	n <- 1 until d
	if 3*n > d && 2*n < d
	if helpers.Helpers.gcd(n, d) == 1
} yield 1

println(result.sum)
