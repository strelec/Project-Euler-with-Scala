val N = 2000000000

def count(size: Long) = size * (size + 1) / 2

def subs(a: Array[Long]): Long = a.size match {
	case 0 => 0
	case 1 => a.head
	case size =>
		val min = a.min
		val pos = a.indexWhere(_ == min)
		val rightSize = size - 1 - pos
		min * (count(size) - count(pos) - count(rightSize)) + subs(a.take(pos)) + subs(a.takeRight(rightSize))
}

val min = 3
val (prefix, period) = {
	val generator = Iterator.iterate(290797L)(s => s * s % 50515093).drop(1)
	val (first, rest) = generator.span(_ != min)
	(first.toArray, rest.drop(1).takeWhile(_ != min).toArray)
}
val periodCount = (N - prefix.size) / (1 + period.size)
val postfix = period.take(N - prefix.size - 1 - periodCount * (period.size + 1))

val base = min * (count(N) - periodCount * count(period.size) - count(prefix.size) - count(postfix.size))
val result = base + periodCount * subs(period) + subs(prefix) + subs(postfix)
println(result)
