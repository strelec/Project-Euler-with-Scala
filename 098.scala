val text = io.Source.fromURL("http://projecteuler.net/project/resources/p098_words.txt").mkString
val words = text.split(",").map(_.tail.init).toVector
val groups = words.groupBy(_.sorted).values.flatMap {
	case Seq() | Seq(_) => Nil
	case Seq(a, b) => List((a, b))
	case a => a.toSet.subsets(2).map( s => (s.head, s.last) )
}

val squares = (1L to 100000).map(x => (x*x).toString).groupBy(_.size)
val result = for {
	(a, b) <- groups
	valid = squares(a.size)
	square <- valid
	dict = (a, square).zipped.toMap
	if a.map(dict) == square &&
		dict.values.size == dict.values.toSet.size &&
		valid.contains(b.map(dict))
	num <- Seq(a.map(dict).toInt, b.map(dict).toInt)
} yield num

println(result.max)
