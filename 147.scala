val N = 47L
val M = 43L

def triangle(n: Long) =
	if (n <= 0) 0 else n * (n + 1) / 2

val result = for {
	a <- 1L to N
	b <- 1L to M
	r <- 0L until a
} yield {
	def compute(sum: Long, lefts: collection.immutable.NumericRange[Long]) = {
		var tr = triangle(sum + 1)
		(for (left <- lefts) yield {
			val right = 2*b - 2 - left
			if (left + right <= sum) (left + 1) * (right + 1)
			else tr - triangle(sum - left) - triangle(sum - right)
		}).sum
	}
	compute(2*r, 1L to 2*b-3 by 2) + compute(2*r-1, 0L to 2*b-2 by 2)
}

println(result.sum + N*(N+1)*(N+2)*M*(M+1)*(M+2)/36)
