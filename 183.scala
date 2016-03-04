val N = 10000

val result = (5 to N).map { n =>
	var k = (n/math.E).round
	k = k / helpers.Helpers.gcd(n, k)
	while (k % 2 == 0) k /= 2
	while (k % 5 == 0) k /= 5
	if (k == 1) -n else n
}.sum

println(result)
