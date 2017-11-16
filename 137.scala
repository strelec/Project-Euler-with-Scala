val N = 15

def generator(xInit: Long, yInit: Long) = {
	var x = xInit
	var y = yInit
	Iterator.continually {
		val xOld = x
		x = -9*x -4*y - 2
		y = -20*xOld -9*y - 4
		xOld
	}.filter(_ > 0).buffered
}

def merger(all: Seq[BufferedIterator[Long]]) =
	Iterator.continually { all.minBy(_.head).next }

val result = merger(Seq(
	generator(0, 1),
	generator(2, 5),
	generator(15, 34)
)).drop(N - 1).next

println(result)
