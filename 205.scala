def throws(a: Range, n: Int): List[Int] =
	if (n == 0)
		List(0)
	else for {
		el <- throws(a, n-1)
		add <- a
	} yield el + add

def distribution(a: Range, n: Int) = {
	val tmp = throws(a, n)
	val size = tmp.size.toDouble
	tmp.groupBy(identity).mapValues(_.size / size)
}

val result = (for {
	peter <- distribution(1 to 4, 9)
	colin <- distribution(1 to 6, 6)
	if peter._1 > colin._1
} yield peter._2 * colin._2).sum

println("%.7f" format result)