val M = 100

val memo = Array.tabulate(M, M)( (i, j) =>
	(1 to i).map( n => ((j+1) * n - 1) / (i+1) ).sum
)

def isSquare(n: Int) = {
	val r = (math.sqrt(n) + 0.5).toInt
	r * r == n
}

val range = memo.indices
var result = 0
for {
	a <- range
	b <- range
	c <- range
	d <- range
	sum = memo(a)(b) + memo(b)(c) + memo(c)(d) + memo(d)(a)
	if isSquare(sum + a + b + c + d + 1)
} result += 1

println(result)
