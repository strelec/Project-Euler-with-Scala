val M = 50
val N = 1000000

def tile(n: Int) = {
	val t = Array.fill(n+2)(1L)
	(M+1 to n+1).foreach { i =>
		t(i) = t(i-1) + (0 to i-M-1).map(t).sum
	}
	t.last
}

val result = Stream.from(M + 5).find(tile(_) > N)
println(result.get)
