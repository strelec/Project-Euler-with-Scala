val N = 1000000 - 1

def powers(a: Int) =
	1 +: helpers.Number.powers(a).map(_.toInt).takeWhile(_ <= N).toArray

val sieve = helpers.Sieve(N)
val numbers = (2 -> powers(2), 3 -> powers(3))

val result = (for {
	((a, pa), (b, pb)) <- Seq(numbers, numbers.swap)
	i <- powers(a).indices
	n <- {
		def f(rem: Int, i: Int, j: Int): List[Int] = {
			if (i == 0) {
				if (a == 2) List(pb(j)) else Nil
			} else {
				val pr = pa(i)*pb(j)
				pr :: (for {
					ii <- (0 until i).toList
					jj <- pb.indices.drop(j+1).takeWhile(pa(ii)*pb(_) <= rem)
					res <- f(rem - pa(ii)*pb(jj), ii, jj)
				} yield res + pr)
			}
		}

		f(N - pa(i), i, 0)
	}
	if sieve.isPrime(n)
} yield n).groupBy(identity).collect {
	case (n, Seq(_)) => n
}.sum

println(result)
