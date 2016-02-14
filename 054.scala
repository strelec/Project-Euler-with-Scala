def value(hand: Seq[(Int, Char)]): Seq[Int] = {
	val sorted = hand.map(_._1).sorted
	val reps = sorted.groupBy(identity).mapValues(_.size)
	val groups = reps.groupBy(_._2).mapValues(
		_.map(_._1).toSeq.sorted.reverse
	)

	val isStraight = sorted.head == sorted.last - 4 || sorted.head == sorted(3) - 3 && sorted.last == 10
	val isFlush = hand.map(_._2).distinct.size == 1

	if (isStraight && isFlush)
		Seq(20, sorted.head)
	else if (groups isDefinedAt 4)
		Seq(19) ++ groups(4) ++ groups(1)
	else if (groups.isDefinedAt(3) && groups.isDefinedAt(2))
		Seq(18) ++ groups(3) ++ groups(2)
	else if (isFlush)
		Seq(17) ++ sorted.reverse
	else if (isStraight)
		Seq(16, sorted.head)
	else if (groups isDefinedAt 3)
		Seq(15) ++ groups(3) ++ groups(1)
	else if (groups.isDefinedAt(2) && groups(2).size == 2)
		Seq(14) ++ groups(2) ++ groups(1)
	else if (groups isDefinedAt 2)
		Seq(13) ++ groups(2) ++ groups(1)
	else
		Seq(12) ++ groups(1)
}

val lines = io.Source.fromURL("https://projecteuler.net/project/resources/p054_poker.txt").getLines
val result = lines.count { line =>
	val (a, b) = line.split(" +").toSeq.map( str =>
		(str.init match {
			case "T" => 10
			case "J" => 11
			case "Q" => 12
			case "K" => 13
			case "A" => 14
			case num => num.toInt
		}) -> str.last
	).splitAt(5)

	import scala.math.Ordering.Implicits._
	value(a) > value(b)
}

println(result)
