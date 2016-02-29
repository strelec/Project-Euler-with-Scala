val N = 50

val t = Array.fill(N+1)(1L)
t(2) = 2
t(3) = 4
(4 to N).foreach { i =>
	t(i) = t(i-1) + t(i-2) + t(i-3) + t(i-4)
}

println(t.last)
