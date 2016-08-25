val N = 1000

def fib(a: BigInt = 1, b: BigInt = 1): Stream[BigInt] = a #:: fib(b, a+b)

val result #:: _ = for {
	(n, i) <- fib().zipWithIndex
	if n.toString.size >= N
} yield i + 1

println(result)
