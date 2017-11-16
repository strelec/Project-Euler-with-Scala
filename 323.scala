val N = 32

val memo = Array.fill(N + 1)(0.0)
for (i <- 1 to N)
	memo(i) = helpers.Helpers.binoms(i).zipWithIndex.map { case (b, j) =>
		b.toDouble * (1 + memo(j))
	}.sum / (math.pow(2, i) - 1)

println("%.10f" format memo.last)
