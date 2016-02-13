val M = 100000

val others = {
	var product = 1L
	val memo = Vector.tabulate(M + 1) { i =>
		if (i % 2 != 0 && i % 5 != 0) {
			product *= i
			product %= M
		}
		product.toInt
	}
	(n: Long) => memo((n%M).toInt) % M
}

def countFactors(n: Long, f: Long) = {
	var f2 = f
	var ret = 0L
	while(f2 <= n) {
		ret += n / f2
		f2 *= f
	}
	ret
}

val N = 1000000000000L

var result = 1L
var div5 = 1L
while(div5 <= N) {
	var x = div5
	while(x <= N) {
		result *= others(N/x)
		result %= M
		x *= 2
	}
	div5 *= 5
}

val difference = countFactors(N, 2) - countFactors(N, 5)
result *= BigInt(2).modPow(difference, M).toLong

println(result % M)
