val message = io.Source.fromURL("https://projecteuler.net/project/resources/p059_cipher.txt").mkString.trim.split(",").map(_.toInt).toVector

val lc = 'a' to 'z'
val vovels = "aeiou".map(_.toInt).toSet
val (_, result) = (
	for {
		a <- lc.iterator
		b <- lc
		c <- lc
		
		m = message.zipWithIndex.map { case (s, i) =>
			i % 3 match {
				case 0 => s ^ a
				case 1 => s ^ b
				case 2 => s ^ c
			}
		}
	} yield m.count(vovels) -> m
).maxBy(_._1)

println(result.sum)
