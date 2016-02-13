import collection.immutable.BitSet

val tries = Seq(
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
).sortBy(-_._2).map {
	case (k, v) => k.toString -> v
}

val positions = Set(0 until 16: _*)

def rec(posib: Vector[BitSet], rem: Seq[(String, Int)]): Option[String] =
	if (rem.isEmpty) Some(posib.map(_.mkString).mkString) else {
		val (seq, corrects) :: tail = rem
		val it = for {
			correct <- positions.subsets(corrects)
			if posib.zipWithIndex.forall { case (set, pos) =>
				val digit = seq(pos) - '0'
				if (correct(pos)) set.contains(digit) else set != Set(digit)
			}
			newposib = posib.zipWithIndex.map { case (set, pos) =>
				val digit = seq(pos) - '0'
				if (correct(pos)) BitSet(digit) else set - digit
			}
			solution <- rec(newposib, tail)
		} yield solution
		if (it.hasNext) Some(it.next) else None
	}

val result = rec(Vector.fill(16)(BitSet(0 to 9: _*)), tries)
println(result)
