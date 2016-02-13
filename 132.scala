
def divisible(reps: Int, num: Int): Boolean = {
	var cur = 0
	var set = collection.mutable.Set.empty[Int]
	(1 to reps).foreach { i =>
		cur = (cur * 10 + 1) % num
		if (cur == 0) {
			return reps % i == 0
		} else if (set(cur)) {
			return false
		}
		set += cur
	}
	false
}


val result = helpers.Sieve(170000).primesIter().filter(
	divisible(1000000000, _)
).take(40)

println(result.sum)
