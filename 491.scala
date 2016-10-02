import helpers.Comb.fixedSubsetSums

val f9 = (2L to 9).product
val p2 = 1L +: helpers.Number.powers(2).take(10).toVector
def count(l: List[Int]) =
	10 * f9 * (10-l.head) * f9 / p2(l.count(_ != 1))

val set = (0 to 9).map(_ -> 2).toList
val result = for {
	sum <- Seq(23, 34, 45, 56, 67)
	subset <- fixedSubsetSums(set, sum, 10)
} yield count(subset)

println(result.sum)
