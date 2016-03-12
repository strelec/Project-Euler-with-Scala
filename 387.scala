val N = 14
val sieve = helpers.Sieve( Seq.fill(N/2)(10).product )

var candidates = (1 to 9).map( i => (i.toLong, i) )
var result = 0L

(2 to N).foreach { _ =>
	candidates = for {
		(n, sum) <- candidates
		feasible = sieve.isPrime(n/sum)
		d <- 0 to 9
		next = 10*n + d
		divisible = next % (sum + d) == 0
		_ = {
			if (
				!divisible && feasible &&
				sieve.isPrime(next)
			) result += next
		}
		if divisible
	} yield (next, sum + d)
}

println(result)
