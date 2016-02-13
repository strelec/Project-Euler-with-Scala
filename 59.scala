val message = io.Source.fromURL("https://projecteuler.net/project/resources/p059_cipher.txt").mkString.trim.split(",").map(_.toInt).toVector

val lc = 'a' to 'z'
val vovels = "aeiou".map(_.toInt).toSet
val result = (
	for {
		a <- lc
		b <- lc
		c <- lc
		
		m = message.zipWithIndex.map {
			case (s, i) if i % 3 == 0 => s ^ a
			case (s, i) if i % 3 == 1 => s ^ b
			case (s, _)               => s ^ c
		}
	} yield m.count(vovels.contains) -> m
).maxBy(_._1)

println(result._2.sum)
