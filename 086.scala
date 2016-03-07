val N = 1000000

val it = {
	val triples = helpers.PythTriple.inOrder.buffered
	val q = collection.mutable.PriorityQueue.empty[(Int, Int)]
	Iterator.continually(q.headOption match {
		case Some((a, b)) if triples.head.mi > -a =>
			q.dequeue
			-a -> b/2
		case _ =>
			val t = triples.next
			q.enqueue(-t.ma -> t.mi)
			t.mi -> (if (2*t.mi > t.ma)
				t.ma/2 - t.ma + t.mi + 1
			else 0)		
	})
}.filter(_._2 != 0)

var soFar = 0
val result = it.dropWhile { case (_, n) =>
	soFar += n
	soFar < N
}

println(result.next._1)
