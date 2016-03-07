val N = 100000000 - 1

def f(a: Int, b: Int): Int =
	if (a + b > N) 0
	else f(3*a + 4*b, 2*a + 3*b) + N/(a + b)

println(f(7, 5))
