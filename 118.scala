val mod = BigInt(10).pow(8)

def exp(a: BigInt, k: BigInt): BigInt =
	if (k == 1) a else
		a.modPow(exp(a, k-1), mod)
		
println(exp(1777, 1855))
