val result = Stream.from(27).filter(_ % 2 == 1).filter { i =>
	var t = (1, 1, 1)
	var seen = Set.empty[(Int, Int, Int)]
	while(!seen(t) && t._3 != 0) {
		seen += t
		t = (t._2, t._3, (t._1 + t._2 + t._3) % i)
	}
	t._3 != 0
}

require(result.head == 27)

println(result(123))
