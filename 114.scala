val N = 50

val t = Array.fill(N+2)(1L)
(4 to N+1).foreach { i =>
	t(i) = t(i-1) + (0 to i-4).map(t).sum
}

println(t.last)
