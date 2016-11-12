val N = 1000000 - 1
val counts = Array.fill(N+1)(0)

for {
	b <- 1 to N
	a <- 5*b/4 until (2*b min (N/b + 5*b)/4+1)
	if 4*a > 5*b
	n = b * (4*a - 5*b)
} counts(n) += 1

println(counts.count(_ == 10))
