val N = 150000000 - 1

def pp(n: BigInt) = n.isProbablePrime(5)

val result = for {
	a <- Seq(10L, 80L, 130L, 200L)
	n <- a to N by 210
	
	m = n % 13
	if m == 1 || m == 3 || m == 4 || m == 9 || m == 10 || m == 12
	
	n2 = BigInt(n*n)
	if pp(n2 + 1) && pp(n2 + 3) && pp(n2 + 7) && pp(n2 + 9) && pp(n2 + 13) && pp(n2 + 27)
	if !pp(n2 + 21)
} yield n

println(result.sum)
