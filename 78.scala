val penta =
	(1 to 500).flatMap { k =>
		val sq = 3*k*k
		Seq(sq-k, sq+k)
	}.map(_ / 2)
	
val N = 60000
val p = Array.fill(N)(BigInt(1))

(1 until N).foreach { n =>
	p(n) = {
		var k = 0
		var sum = BigInt(0)
		while(penta(k) <= n) {
			val sign = if (k / 2 % 2 == 0) 1 else -1
			sum += sign * p(n - penta(k))
			k += 1
		}
		sum
	}
}

val result = p.indexWhere(_ % 1000000 == 0)

println(result)
