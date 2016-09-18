val Ns = Seq(7208785, 5678027)

def f(r: Int, c: Int) =
	if (0 to r contains c) 1L*r*(r+1)/2 + 1 + c else 3

def isPrime(n: Long) =
	n % 3 != 0 && n % 5 != 0 && n % 7 != 0 && n % 11 != 0 &&
	BigInt(n).isProbablePrime(5)

def checkT(a: Long, b: Long, c: Long) =
	if (!isPrime(a)) 0
	else if (!isPrime(b) && !isPrime(c)) 1
	else 2

def sum(r: Int) =
	(for {
		c <- (r/2%2 to r by 2).iterator
		n = f(r, c)
		if isPrime(n) &&
			checkT(f(r-1, c  ), f(r-2, c-1), f(r-2, c+1)) +
			checkT(f(r+1, c-1), f(r+2, c-1), f(r, c-2)) +
			checkT(f(r+1, c+1), f(r+2, c+1), f(r, c+2)) >= 2
	} yield n).sum

val result = Ns.map(_ - 1).map(sum)
println(result.sum)
