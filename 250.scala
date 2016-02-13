import helpers._

val mod = BigInt(10).pow(16)

val counts = (1 to 250250).map { i =>
	BigInt(i).modPow(i, 250)
}.groupBy(_.toInt).mapValues(_.size).withDefaultValue(0)

def prodRange(range: Range) =
	range.foldLeft(BigInt(1))(_ * _ % mod)


var memo = collection.mutable.Map.empty[(Int, Int), BigInt]

def count(rem: Int, cur: Int): BigInt = {
	val number = counts(cur)
	if (cur == 1)
		prodRange(number until number - rem by -1)
	else memo.getOrElse((rem, cur), {
		val ret = (for {
			i <- 0 to rem/cur
			pos = prodRange(number until number - i by -1)
			call = count(rem - i*cur, cur - 1)
		} yield call * pos).sum % mod
		memo((rem, cur)) = ret
		ret
	})
}

println(counts)
println(count(0, 1))
println(count(250, 250))
