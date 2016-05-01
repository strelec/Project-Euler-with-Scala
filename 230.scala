val N = 17
val A = "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
val B = "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"
require(A.size == B.size)

val fib = (1 to 91).scanLeft(1L -> 1L) {
	case ((a, b), _) => (b, a + b)
}.map(_._1)

def get(n: Long, order: Int): Char = order match {
	case 0 => A(n.toInt)
	case 1 => B(n.toInt)
	case _ =>
		val n2 = n / A.size
		if (n2 < fib(order - 2))
			get(n, order - 2)
		else
			get(n - fib(order - 2) * A.size, order - 1)
}

val result = helpers.Number.powers(7).zipWithIndex.take(N).map { case (p, i) =>
	val n = (127L + 19*(i+1)) * p
	get(n - 1, fib.size)
}.mkString

println(result.reverse + get(127 - 1, 20))
