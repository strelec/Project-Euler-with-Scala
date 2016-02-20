/*
	EXPLANATION:
	
	1. This is a simple depth first search with rigorous pruning. The state of the search is for each digit the remaining values.
	2. In each function call, completely ignore the digits on the right places. Moreover, if there are more such digits than allowed, prune.
	3. See how many digits need to be correct atop of those already correct and choose all that big subsets from available digits.
	
	NOTE: This solution runs more than 5 seconds, but finishes below a minute.
*/

import collection.immutable.BitSet

val tries = List(
	5616185650518293L -> 2,
	3847439647293047L -> 1,
	5855462940810587L -> 3,
	9742855507068353L -> 3,
	4296849643607543L -> 3,
	3174248439465858L -> 1,
	4513559094146117L -> 2,
	7890971548908067L -> 3,
	8157356344118483L -> 1,
	2615250744386899L -> 2,
	8690095851526254L -> 3,
	6375711915077050L -> 1,
	6913859173121360L -> 1,
	6442889055042768L -> 2,
	2321386104303845L -> 0,
	2326509471271448L -> 2,
	5251583379644322L -> 2,
	1748270476758276L -> 3,
	4895722652190306L -> 1,
	3041631117224635L -> 3,
	1841236454324589L -> 3,
	2659862637316867L -> 2
).sortBy(_._2).map {
	case (k, v) => k.toString -> v
}

def rec(posib: IndexedSeq[BitSet], rem: List[(String, Int)]): Iterator[String] =
	rem match {
		case Nil => Iterator(posib.flatten.mkString)
		case (seq, required) :: tail =>
			var already = 0
			val candidates = (0 until 16).filter( i =>
				posib(i)(seq(i) - '0') && (
					posib(i).size > 1 || {already += 1; false})
			).toSet
			for {
				correct <- candidates.subsets(required - already)
				newposib = (0 until 16).map { i =>
					val c = seq(i) - '0'
					if (posib(i).size == 1) posib(i)
					else if (correct(i)) BitSet(c)
					else posib(i) - c
				}
				solution <- rec(newposib, tail)
			} yield solution
	}

val full = Vector.fill(16)( BitSet(0 to 9: _*) )
val result = rec(full, tries).next
println(result)
