val BEFORE = "00000000137"
val AFTER = "56789"

val candidates = {
	val postfix = (AFTER + BEFORE.head).reverse.map(_.asDigit).toArray
	def rightEnding(p: Int) = {
		val invOf10 = BigInt(10).modInverse(p).toInt
		var r = 1L
		postfix.forall { digit =>
			val expected = 10 * r / p
			r = invOf10 * r % p
			expected == digit
		}
	}

	val f = s"0.$BEFORE".toDouble
	val range = (1/(f + math.pow(10, -BEFORE.size))).toInt to (1/f).toInt
	range.iterator.filter(BigInt(_).isProbablePrime(3)).filter(rightEnding)
}

def digitSum(p: Int) = {
	var t = 0
	var r = 1L
	var sum = 0L
	do {
		t += 1
		sum += 10 * r / p
		r = (10 * r) % p
	} while (r != 1)
	if (t == p - 1) sum else -1
}

val result = candidates.map(digitSum).find(_ != -1)
println(result.get)
