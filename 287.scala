val N = 24

def isBlack(x: Int, y: Int) =
	x.toLong*x + y.toLong*y <= (1L << 2*N - 2)

def compute(x1: Int, x2: Int, y1: Int, y2: Int): Int =
	if (isBlack(x1, y1) == isBlack(x2, y2)) 2
	else {
		val x3 = (x1 + x2)/2
		val y3 = (y1 + y2)/2
		1 + compute(x1, x3, y1, y3) +
			compute(x3 + 1, x2, y3 + 1, y2) +
			compute(x1, x3, y3 + 1, y2) +
			compute(x3 + 1, x2, y1, y3)
	}

val max = (1 << N - 1) - 1
val result =
	1 + compute(0, max, 0, max) +
		2 * compute(0, max, 1, max + 1) +
		compute(1, max + 1, 1, max + 1)
println(result)
