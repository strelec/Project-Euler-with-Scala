val squares = Vector(0->1, 0->4, 0->9, 1->6, 2->5, 3->6, 4->9, 6->4, 8->1)

def augment(v: IndexedSeq[Int]) = {
	val bs = collection.immutable.BitSet(v: _*)
	if (v contains 6) bs + 9
	else if (v contains 9) bs + 6
	else bs
}

val combs = (0 to 9).combinations(6).map(augment).toVector

val result = for {
	first <- combs
	second <- combs
	
	if squares.forall { case (f, s) =>
		first.contains(f) && second.contains(s) ||
		first.contains(s) && second.contains(f)
	}
} yield (first, second)

println(result.size / 2)
