import scala.collection.Searching._

val N = 28123

val sieve = sieves.DivisorSum(N)

val abundant = (1 to N).filter( i =>
	sieve(i) > 2*i
)

val result = (1 to N).filter( i =>
	abundant.takeWhile(_ < i).forall( j =>
		abundant.search(i - j) match {
			case Found(_) => false
			case _ => true
		}
	)
)

println(result.sum)
