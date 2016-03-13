def p(a: Int, p: Int) = BigInt(a).pow(p)

val result = (3 to 16).map(_ - 1).map( n =>
	15*p(16,n) - 43*p(15,n) + 41*p(14,n) - p(13,n+1)
).sum

println(result.toString(16).toUpperCase)
