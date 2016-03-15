val opts = Seq(25, 50) ++ (for {
	i <- 1 to 20
	j <- Seq(i, 2*i, 3*i)
} yield j)

def f(rem: Int, places: Int, below: Int): Int = places match {
	case 0 => if (rem < 0) 0 else 1
	case n => (for {
		(opt, i) <- opts.take(below).zipWithIndex
	} yield f(rem - opt, places - 1, i+1)).sum
}

val result = for {
	places <- 0 to 2
	opt <- (2 to 40 by 2) :+ 50
} yield f(99 - opt, places, 10000)

println(result.sum)
