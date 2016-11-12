val N = 1000000000000L
val K = 10000

val primes = {
	val P = 2000000000
	val sieve = helpers.Sieve(100000)
	(P to P+2000).filter( p => sieve.isPrime(p.toInt) )
}

def f(n: Int, k: Int, p: Int) = {
	var b = (BigInt(n) until n-k-1 by -1).reduce(_ * _ % p)
	(2 to k+1).foreach { i =>
		b *= BigInt(i).modInverse(p)
	}
	b %= p
	
	val powers = Array.tabulate(k+1)(
		BigInt(_).modPow(k, p).toLong
	)

	val binoms =
		helpers.Helpers.binoms(k+1).map(_ % p).map(_.toLong).toArray

	(k to (1 max n-p+k+1) by -1).map { i =>
		b *= n-i+k+1
		b *= BigInt(n-i).modInverse(p)
		
		b * (0 to i-1).map( j =>
			powers(i - j) * binoms(j) *
			(if (j % 2 == 0) 1 else -1) % p
		).reduce(_ + _ % p) % p
	}.sum % p
}

val result = primes.par.map { p =>
	val n = (N % p).toInt
	val tmp = ((n+1) * f(n, K, p) - f(n, K+1, p)) % p
	if (tmp < 0) tmp + p else tmp
}

println(result.sum)
