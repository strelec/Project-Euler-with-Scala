case class State(x: Int, y: Int, carrying: Boolean, seeds: Int) {
	def isFinal = seeds == 31
	
	def neighbours = {
		var result = List.empty[State]
		def move(x: Int, y: Int) =
			if (x >= 0 && x < 5 && y >= 0 && y < 5) {
				result ::= (
					if (carrying && x == 0 && (seeds & 1 << y) == 0)
						State(x, y, false, seeds | 1 << y)
					else if (!carrying && x == 4 && (seeds & 1 << y + 5) != 0)
						State(x, y, true, seeds & ~(1 << y + 5))
					else State(x, y, carrying, seeds))
			}

		move(x-1, y)
		move(x+1, y)
		move(x, y-1)
		move(x, y+1)
		result
	}
}

def step(snapshot: collection.Map[State, Double]) = {
	val result = collection.mutable.Map.empty[State, Double].withDefaultValue(0.0)
	var pFinal = 0.0
	
	for {
		(state, p) <- snapshot
		neighbours = state.neighbours
		state <- neighbours
	} {
		val p2 = p / neighbours.size
		if (state.isFinal) pFinal += p2
		else result(state) += p2
	}
	(result, pFinal)
}

var cur =  collection.Map(State(2, 2, false, 31 << 5) -> 1.0)
var result = 0.0
for (i <- 1 to 3000) {
	val (curTemp, p) = step(cur)
	cur = curTemp
	result += p * i
}
println("%.6f" format result)
