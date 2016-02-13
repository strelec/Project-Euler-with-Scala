val result = for {
	i1 <- 10 until 100
	n1 = 10 * i1 + 0
	if n1*n1 / 100 % 10 == 9

	i2 <- 10 until 100
	n2 = 1000L * i2 + n1
	if n2*n2 / 10000 % 10 == 8

	i3 <- 10 until 100
	n3 = 100000L * i3 + n2
	if n3*n3 / 1000000 % 10 == 7

	i4 <- 10 until 100
	n4 = 10000000L * i4 + n3
	if n4*n4 / 100000000 % 10 == 6

	// 3 already overflows Long (2⁶³-1)
	i5 <- 1 until 3
	n = 1000000000L * i5 + n4
	n2 = n * n

	if n2 / 10000000000L % 10 == 5
	if n2 / 1000000000000L % 10 == 4
	if n2 / 100000000000000L % 10 == 3
	if n2 / 10000000000000000L % 10 == 2
	if n2 / 1000000000000000000L == 1
} yield (n, n2)

println(result.head._1)