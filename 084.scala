import util.Random.nextInt

def advance(i: Int): Int = i % 40 match {
	case f @ (2 | 17 | 33) =>
		nextInt(16) match {
			case 1 => 0  // GO
			case 2 => 10 // JAIL
			case _ => f
		}
	case f @ (7 | 22 | 36) =>
		nextInt(16) match {
			case 1 => 0  // GO
			case 2 => 10 // JAIL
			case 3 => 11 // C1
			case 4 => 24 // E3
			case 5 => 39 // H2
			case 6 => 5  // R1
			case 7 | 8 => // next railway
				((f + 5) / 10 * 10 + 5) % 40
			case 9 => // next utility
				if (f > 28 || f < 12) 12 else 28
			case 10 =>
				advance(f - 3)
			case _ => f
		}
	case 30 => 10
	case f => f 
}

val lands = Array.fill(40)(0)
var pos = 0
var doubles = 0
(0 to 1000000).foreach { _ =>
	lands(pos) += 1
	val a = nextInt(4) + 1
	val b = nextInt(4) + 1
	doubles = if (a == b) doubles + 1 else 0
	pos = if (doubles == 3) {
		doubles = 0
		10
	} else advance(pos + a + b)
}

val result = lands.zipWithIndex.sorted.reverse.take(3)
println(result.map(_._2).mkString)
