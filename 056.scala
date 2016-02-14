def range = (1 until 100).toIterator

val result = for {
	a <- range
	b <- range
} yield BigInt(a).pow(b).toString.map(_.asDigit).sum

println(result.max)