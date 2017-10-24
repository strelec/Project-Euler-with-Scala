val N = 10000

val (pairs, squares, indices) = {
	val singles = {
		def f(k: Int) = math.exp(1.0 * k / N) - 1
		val maxn = Iterator.from(N).find(f(_) > math.Pi).get - 1
		Array.tabulate(maxn)( k => f(k+1) )
	}

	val maxsize = N * N * 3/4
	val pairs = Array.fill(maxsize)(0.0)
	val squares = Array.fill(maxsize)(0)
	var size = 0
	for {
		i <- singles.indices
		j <- singles.indices.drop(i)
		a = singles(i)
		b = singles(j)
		if a + b < math.Pi
	} {
		pairs(size) = a + b
		squares(size) = (i+1)*(i+1) + (j+1)*(j+1)
		size += 1
	}

	val indices = (0 until size).toArray.sortWith( (i, j) => pairs(i) < pairs(j) )
	(pairs, squares, indices)
}

var best = 100.0
var result = 0
var i = 0
var j = indices.size - 1
while (i <= j) {
	val cur = pairs(indices(i)) + pairs(indices(j))
	val diff = math.abs(math.Pi - cur)
	if (diff < best) {
		best = diff
		result = squares(indices(i)) + squares(indices(j))
	}
	if (cur > math.Pi) j -= 1 else i += 1
}

println(result)
