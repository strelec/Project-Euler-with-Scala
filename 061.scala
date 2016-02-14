val fs: Vector[Int => Int] = Vector(
	n => n * (n+1) / 2,
	n => n * n,
	n => n * (3*n-1) / 2,
	n => n * (2*n-1),
	n => n * (5*n-3)/2,
	n => n * (3*n-2)
)

val buckets = fs.map( f =>
	Stream.from(1).map(f).dropWhile(_ < 1000).takeWhile(_ < 10000).toVector
).reverse

def matches(a: Int, b: Int) = a % 100 == b / 100

def rec(prev: Int, rem: Set[Int]): Seq[List[Int]] =
	if (rem.isEmpty) Seq(Nil) else for {
		i <- rem.toSeq
		code <- buckets(i)
		if matches(prev, code)
		ret <- rec(code, rem - i)
	} yield code :: ret

val rem = (0 to 4).toSet
val result = for {
	code <- buckets(5)
	ret <- rec(code, rem)
	if matches(ret.last, code)
} yield code :: ret

println(result.head.sum)
