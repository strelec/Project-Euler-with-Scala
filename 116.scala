val N = 50

def tile(n: Int) = {
	val t = Array.fill(N+1)(1L)
	(n to N).foreach { i =>
		t(i) = t(i-1) + t(i-n)
	}
	t.last
}

val result = tile(2) + tile(3) + tile(4) - 3
println(result)
