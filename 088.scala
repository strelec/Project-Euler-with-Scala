val N = 12000

val sieve = sieves.Factors(800000)

def ks(n: Int) = {
	val factors = sieve(n)
	
	def aux(n: Int, cur: Int): Set[(Int, Int)] = if (n == 1) Set((cur, 1)) else {
		val cont = (for {
			p <- factors
			if n % p == 0
			prev <- aux(n/p, cur*p)
		} yield prev).toSet
		
		if (cur == 1) cont else aux(n, 1).map { case (a, b) => (a+cur, b+1) } ++ cont
	}

	aux(n, 1).collect {
		case (a, b) if b != 1 =>
			n - a + b 
	}
}

val result = Array.fill(N+1)(-1)
result(0) = 0
result(1) = 0

for {
	n <- 4 to 12200
	k <- ks(n)
	if k <= N
	if result(k) == -1
} result(k) = n

require(result.forall(_ != -1))
println(result.distinct.sum)

