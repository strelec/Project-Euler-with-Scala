val sets = io.Source.fromURL("https://projecteuler.net/project/resources/p105_sets.txt").getLines.map(
	_.split(",").map(_.toInt).toSet
).toList

val result = sets.filterNot( set =>
	(for {
		a <- set.subsets.drop(1)
		b <- (set &~ a).subsets.drop(1)
		if a.sum == b.sum ||
			a.size > b.size && a.sum <= b.sum
	} yield 1).nonEmpty
)

println(result.flatten.sum)
