val str = io.Source.fromURL("https://projecteuler.net/project/resources/p022_names.txt").mkString.tail.init
val names: Seq[String] = str.split("\"?,\"?")

val sol = names.sorted.zipWithIndex.map { case (x, i) =>
	(1+i) * x.toSeq.map(_ - 'A' + 1).sum
}.sum

println(sol)
