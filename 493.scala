def binomial(n: Int, k: Int) = {
	(BigInt(n - k + 1) to n).product /
	(BigInt(1) to k).product
}

val result = 7*(1 - binomial(60, 20).toDouble/binomial(70, 20).toDouble)
println("%.9f" format result)
