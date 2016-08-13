val N = 150000000

val result = for {
	k <- 0L to N/70
	n <- Seq(70*k + 10, 70*k + 60)
	if n < N
	
	if n % 3 != 0
	m = n % 13
	if m == 1 || m == 3 || m == 4 || m == 9 || m == 10 || m == 12
	
	n2 = BigInt(n*n)
	if (n2 + 1).isProbablePrime(5)
	if (n2 + 3).isProbablePrime(5)
	if (n2 + 7).isProbablePrime(5)
	if (n2 + 9).isProbablePrime(5)
	if (n2 + 13).isProbablePrime(5)
	if (n2 + 27).isProbablePrime(5)
	
	if !(n2 + 19).isProbablePrime(5)
	if !(n2 + 21).isProbablePrime(5)
} yield n

println(result.sum)
