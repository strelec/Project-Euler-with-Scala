val str = io.Source.fromURL("https://projecteuler.net/project/resources/p042_words.txt").mkString.trim.tail.init
val words: Seq[String] = str.split("\",\"")

val isTriangle = (1 to 100).map(n => n*(n+1)/2).toSet

val result = words.filter( word =>
	isTriangle(word.map(_ - 'A' + 1).sum)
)

println(result.size)
