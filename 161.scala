val N = 9

val shapes = Seq((N, N+1), (1, N), (N-1, N), (1, N+1), (1, 2), (N, N+N))
def isValid(a: Int, i: Int) = if (a == N - 1) i != 0 else a/N == (a+i)/N

def step(a: Array[Long]) = {
	val result = Array.fill(1 << 2*N)(0L)

	for ((c, i) <- a.zipWithIndex if c != 0) {
		def aux(state: Int, i: Int): Unit =
			if (i == N) result(state >> N) += c else {
				def isFree(i: Int) = (state & 1 << i) == 0
				if (!isFree(i)) aux(state, i + 1)
				else for {
					(a, b) <- shapes
					if isFree(a + i) && isFree(b + i)
					if isValid(a, i) && isValid(b, i)
				} aux(state | 1 << a + i | 1 << b + i, i + 1)
			}
		aux(i, 0)
	}
	result
}

var state = Array(1L)
for (_ <- 1 to 12) state = step(state)
println(state(0))
