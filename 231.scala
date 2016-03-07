val N = 20000000
val C = 15000000

val primes = helpers.Sieve(N).primes

def upTo(n: Long) = (for {
	p <- primes.takeWhile(_ <= n)
	pp <- helpers.Number.powers(p).takeWhile(_ <= n)
} yield n/pp*p).sum

println(upTo(N) - upTo(N-C) - upTo(C))
