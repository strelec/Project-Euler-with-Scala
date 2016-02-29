/*
	EXPLANATION:
	
	1. It is known that a Langton's ant has a period of length 104 that starts occuring somewhere around 10,000th step
	2. We compute where we have stop to have a whole number of 104-cycles left
	3. Simulate the ant for 12,000 steps and compute the surplus of each 104-cycle
	4. Add that amount of black fields for each cycle to the simulation result
*/

val N = 1000000000000000000L

val period = 104
val repeats = (N - 12000)/period
val base = N - repeats*period

var dir = 0
var pos = (0, 0)
var blacks = Set.empty[(Int, Int)]
var prev = 0

(1L to base).foreach { i =>
	if (blacks.contains(pos)) {
		blacks -= pos
		dir += 3
		dir %= 4
	} else {
		blacks += pos
		dir += 1
		dir %= 4
	}
	
	val (x, y) = pos
	pos = dir match {
		case 0 => (x, y+1)
		case 1 => (x+1, y)
		case 2 => (x, y-1)
		case 3 => (x-1, y)
	}
	
	if (i == base - period)
		prev = blacks.size
}

val result = blacks.size + (blacks.size - prev)*repeats
println(result)
