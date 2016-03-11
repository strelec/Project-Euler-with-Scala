val N = 1000000000000L
val K = 10000

val P = 2000000000
val sieve = helpers.Sieve(100000)
val primes = (P.toLong to P+2000).filter(sieve.isPrime)

def f(n: Long, k: Int, p: Long) = {
	//var b = (BigInt(n+k+1) until n by -1).reduce(_ * _ % p)
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

	(k to 1 by -1).map { i =>
	//(1 to k).map { i =>
		//b *= n-i+1
		b *= n+k-i+1
		if (b != 0) {
			//b *= BigInt(n-i+k+2).modInverse(p)
			b *= BigInt(n-i).modInverse(p)
			b %= p
			
			b * (0 to i-1).map( j =>
				powers(i - j) * binoms(j) *
				(if (j % 2 == 0) 1 else -1) % p
			).reduce(_ + _ % p)
		} else {
			println("ZERO", n,i,k,p)
			BigInt(0)
		}
	}.sum % p
}

//println(f(100, 4, 2147483647))


lazy val result = primes.take(3).par.map { p =>
	val tmp = ((N+1)*f(N%p, K, p) - f(N%p, K+1, p)) % p
	println(tmp)
	if (tmp < 0) tmp + p else tmp
}

println(result.sum)
