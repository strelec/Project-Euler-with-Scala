val primes = helpers.Sieve(100).primes.reverse

def recurse(rem: Int, i: Int = 0): Int =
	primes.lift(i) match {
		case Some(prime) =>
			(0 to rem/prime).map( times =>
				recurse(rem - times*prime, i + 1)
			).sum
			
		case _ => if (rem == 0) 1 else 0
	}

val result = (1 to 100).find(recurse(_) > 5000)

println(result.get)
