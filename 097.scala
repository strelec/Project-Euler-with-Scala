val mod = 10000000000L

var num = 1L

for (i <- 1 to 7830457) {
	num *= 2
	num %= mod
}

val result = 28433*num + 1
println(result % mod)