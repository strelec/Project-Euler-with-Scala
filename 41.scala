def isPrime(n: Long) = (2 to math.sqrt(n).toInt).forall(n % _ != 0)

val result = for {
	n <- (9 to 1 by -1).toStream
	i <- (n to 1 by -1).permutations
	p = i.mkString.toLong
	if isPrime(p)
} yield p

println(result.head)