val nom = 15499L
val denom = 94744L

val primes = helpers.Sieve(25).primes.map( p =>
	BigInt(p) -> 1
) ++ helpers.Sieve(5).primes.flatMap( p =>
	List(BigInt(p) -> 2, BigInt(p) -> 3)
)

val result = primes.toSet.subsets.drop(1).flatMap { set =>
	val s = set.toSeq.map(_._1)
	val n = s.product
	val res = denom * (n / set.map(_._1).product) * s.map(_ - 1).product / (n - 1)
	if (res < nom) Some(n -> res) else None 
}.min

println(result)
