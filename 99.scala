val data = io.Source.fromURL("https://projecteuler.net/project/resources/p099_base_exp.txt").getLines.map(_.split(",").map(_.toInt))

val result = data.zipWithIndex.maxBy {
	case (Array(a, b), i) => b * math.log(a)
}

println(result._2 + 1)
