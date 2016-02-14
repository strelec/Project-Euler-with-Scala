val matrix = io.Source.fromURL("https://projecteuler.net/project/resources/p083_matrix.txt").getLines.map(
	line => line.split(",").map(_.toInt)
).toVector

var dist = Map((0, 0) -> matrix(0)(0))
val q = collection.mutable.PriorityQueue(dist(0, 0) -> (0, 0))(
  implicitly[Ordering[(Int, (Int, Int))]].reverse
)

def adj(p: (Int, Int)) = {
	val (x, y) = p
	for {
		np <- Vector((x, y+1), (x+1, y), (x-1, y), (x, y-1))
		vx <- matrix.lift(np._1)
		vy <- vx.lift(np._2)
	} yield np -> vy
}

while(q.nonEmpty) {
	val (_, z) = q.dequeue
	adj(z).foreach { case (c, cost) =>
		val predicted = dist(z) + cost
		if (!dist.contains(c) || dist(c) > predicted) {
			dist += c -> predicted
			q.enqueue(predicted -> c)
		}
	}
}
val result = dist(matrix.size - 1, matrix.head.size - 1)
println(result)
