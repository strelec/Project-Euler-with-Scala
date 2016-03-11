val N = 1000000000000L
val K = 10000

val P = 2000000000
val sieve = helpers.Sieve(100000)
val primes = (P.toLong to P+2000).filter(sieve.isPrime)

def f(n: Long, k: Int, p: Long): BigInt = {
	var b = (BigInt(n+k+1) until n by -1).reduce(_ * _ % p)
	var b = (BigInt(n+1) until n-k by -1).reduce(_ * _ % p)
	if (b == 0) return 1
	(2 to k+1).foreach { i =>
		b *= BigInt(i).modInverse(p)
	}
	b %= p
	
	var b = (BigInt(n+k+1) until n by -1).product / (BigInt(2) to k+1).product % p
	
	val powers = Array.tabulate(k+1)(
		BigInt(_).modPow(k, p).toLong
	)

	val binoms =
		helpers.Helpers.binoms(k+1).map(_ % p).map(_.toLong).toArray

	(1 to k).map { i =>
		b *= n-i+1
		if ((n-i+k+2)%p != 0) {
			b *= BigInt(n-i+k+2).modInverse(p)
			b %= p
		} else b = 0
		val b2 = (BigInt(n-i+k+1) until n-i by -1).product / (BigInt(2) to k+1).product % p
		if (b != b2) println(b, b2, "ERROR")
		
		b * (0 to i-1).map( j =>
			powers(i - j) * binoms(j) *
			(if (j % 2 == 0) 1 else -1) % p
		).reduce(_ + _ % p)
	}.sum % p
}

def test = {
	val n = 100L
	val k = 50
	val p = 23
	
	println((1 to 5000).filter { n =>
		val a1 = (1L to n).map(BigInt(_).modPow(k, p)).sum % p
		val tmp = f(n, k, p)
		val a2 = if (tmp < 0) tmp + p else tmp
		println(n, a1, a2)
		a1 != a2
	})
	
}



lazy val result = primes.par.map { p =>
	val tmp = ((N+1)*f(N, K, p) - f(N, K+1, p)) % p
	println(tmp)
	if (tmp < 0) tmp + p else tmp
}

println(result.sum)
