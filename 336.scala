val N = 11
val M = 2011

def isMaximix(a: Seq[Int], i: Int): Boolean =
	(a.size == 2 && a.head != i) ||
	(a.head != i && a.last != i) && {
		val pos = a.indexOf(i)
		isMaximix(a.drop(pos+1) ++ a.take(pos).reverse, i+1)
	}

def isMaximix(a: Seq[Int]): Boolean = isMaximix(a, 0)

val result = (0 until N).permutations.filter(isMaximix).drop(M-1).next
println(result.map('A' + _).map(_.toChar).mkString)
