val N = BigInt(10).pow(17).toLong - 1

def fib(a: Long = 1, b: Long = 2): Stream[Long] =
	a #:: fib(b, a+b)

val f = fib().takeWhile(_ <= N).toVector

val w = Array.tabulate(f.size)(_.toLong)
for(i <- w.indices.drop(3))
	w(i) = w(i-1) + w(i-2) + f(i-2)

def g(n: Long, k: Int): Long =
	if (n <= 3) n
	else if (n <= f(k)) g(n, k-1)
	else
		w(k) + // g(f(k) - 1, k-1)
		g(n - f(k), k-2) +
		n - f(k) + 1

println(g(N, f.size-1))
