val pows = Vector.tabulate(200)(BigInt(2).pow)

def rec(rem: BigInt, p: Int): Int = {
	if (rem < 0 || rem > pows(p+2) - 2) 0
	else if (p == 0) 1
	else rec(rem, p-1) + rec(rem - pows(p), p-1) + rec(rem - pows(p+1), p-1)
}



val result = rec(BigInt(10).pow(17), 100)
println(result)


