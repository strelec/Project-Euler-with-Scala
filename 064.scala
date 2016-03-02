val N = 10000

def isSquare(n: Int) = {
	val r = (math.sqrt(n) + 0.5).toInt
	r*r == n
}

def seq(r: Int) = {
	var seen = Map.empty[(Int, Int), Int]
	var state = (0, 1)
	var i = 0
	while (!seen.contains(state)) {
		seen += state -> i
		i += 1
		
		val (m, d) = state
		val a = ((math.sqrt(r) + m)/d).toInt
		val nm = d*a - m
		state = (nm, (r - nm*nm)/d)
	}
	i - seen(state)
}

val result = (2 to N).filterNot(isSquare).count(seq(_) % 2 == 1)
println(result)
