def binomial(n: Int, k: Int) = {
	(BigInt(n - k + 1) to n).product /
	(BigInt(1) to k).product
}

val n = 20
println(binomial(2*n, n))