val MOD = 100000007

object State {
	val size = 1 << 19

	def empty = State(Array.fill(size)(0), Array.fill(size)(0L))

	def initial(start: Int) = {
		val state = empty
		state.count(start) = 1
		state
	}
}

case class State(count: Array[Long], sum: Array[Long]) {
	def rot4r(i: Int, pos: Int) = {
		val three = ((i >> (pos + 1)) & 7) << pos
		val one = ((i >> pos) & 1) << (pos + 3)
		(i & ~(15 << pos)) | three | one
	}

	def rot4l(i: Int, pos: Int) =
		rot4r(rot4r(rot4r(i, pos), pos), pos)

	def next = {
		val result = State.empty
		for ((c, i) <- count.zipWithIndex if c > 0) {
			val prev = sum(i) * 243
			def update(dir: Char, i: Int) {
				result.count(i) += c
				result.count(i) %= MOD
				result.sum(i) += prev + c * dir
				result.sum(i) %= MOD
			}

			val white = i % 16
			if (white % 4 != 0)
				update('D', i - 1)
			if (white % 4 != 3)
				update('U', i + 1)
			if (white >= 4)
				update('R', rot4r(i, white) - 4)
			if (white < 12)
				update('L', rot4l(i, white + 4) + 4)
		}
		result
	}
}

var state = State.initial(127 << 4)
val end = Integer.parseInt("101001011010010", 2) << 4
while(state.sum(end) == 0)
	state = state.next

println(state.sum(end))
