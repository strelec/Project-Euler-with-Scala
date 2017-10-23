val N = 1000000
val M = 20000

val width = M/2 + 2
var prev = Array.fill(width)(1.0)
var next = Array.fill(width)(0.0)

for (n <- 1 to M) {
	for (i <- 0 to (M - n)/2) {
		val a = N - M + n + i
		val b = M - n - 2*i
		next(i) = (a*prev(i) + b*prev(i + 1)) / N
	}
	val swap = next
	next = prev
	prev = swap
}

println("%.10f" format 1 - prev.head)
