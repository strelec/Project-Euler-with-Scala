def fib(a: BigInt = 1, b: BigInt = 1): Stream[BigInt] = a #:: fib(b, a+b)

val result = fib().zipWithIndex.find(_._1.toString.size >= 1000)

println(result.get._2 + 1)