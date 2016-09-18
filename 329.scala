val N = 500
val S = "PPPPNNPPPNPPNPN"

val sieve = helpers.Sieve(N)
var memo = Array.fill(N)(1L)

for (b <- S.reverse.map(_ == 'P')) {
	memo = Array.tabulate(N) { i =>
		(i+1 match {
			case 1 => memo(1) * 2
			case N => memo(N-2) * 2
			case _ => memo(i-1) + memo(i+1)
		}) * (if (sieve.isPrime(i+1) == b) 2 else 1)
	}
}

val nom = memo.sum
val denom = N * BigInt(6).pow(S.size)
val gcd = denom.gcd(nom).toLong
println(nom/gcd + "/" + denom/gcd)
