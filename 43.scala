val primes = Seq(2, 3, 5, 7, 11, 13, 17)

val result = (0 to 9).permutations.filter { x =>
	val consecutive = x.tail.sliding(3).map(_.mkString.toInt)
	x.head != 0 && consecutive.toSeq.zip(primes).forall {
		case (a, b) => a % b == 0
	}
}.map(_.mkString.toLong).sum

println(result)