val N = 20000000
val M = 10000000

val primes = helpers.Sieve(N).primesIter(M)

val digits = Vector(
	Vector(1,1, 1,0,1, 1,1),
	Vector(0,0, 0,0,0, 1,1),
	Vector(0,1, 1,1,1, 1,0),
	Vector(0,0, 1,1,1, 1,1),
	Vector(1,0, 0,1,0, 1,1),
	Vector(1,0, 1,1,1, 0,1),
	Vector(1,1, 1,1,1, 0,1),
	Vector(1,0, 1,0,0, 1,1),
	Vector(1,1, 1,1,1, 1,1),
	Vector(1,0, 1,1,1, 1,1)
)

val keepers = Vector.tabulate(10, 10)( (i, j) =>
	(digits(i), digits(j)).zipped.count {
		case (a, b) => 1 == a && 1 == b
	}
)

def root(l: List[Int]) =
	helpers.Number.digits(l.sum).reverse

val result = for {
	p <- primes
	ps = root(List(p))
	it = Iterator.iterate(ps)(root).takeWhile(_.nonEmpty)
	Seq(a, b) <- it.sliding(2).takeWhile { case Seq(a, b) => a != b }
	v <- (a, b).zipped.map(keepers(_)(_))
} yield v

println(2 * result.sum)
