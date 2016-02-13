val products = List(1L,1,2,6)

def sums(sum: Int, rem: Int = 9): Seq[List[Long]] =
	(sum, rem) match {
		case (0, 0) => Seq(List())
		case (_, 0) => Seq()
		case _ => for {
			how <- 0 to (if (rem == 1) 2 else 3)
			tail <- sums(sum - how, rem - 1)
		} yield products(how) :: tail 
	}

val f17 = BigInt((2L to 17).product)

val result = sums(17).map(f17 / _.product)

println(9 * result.sum)
