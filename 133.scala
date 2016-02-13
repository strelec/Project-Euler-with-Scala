def isTen(a: Int) = {
	var b = a
	while(b % 2 == 0) b /= 2
	while(b % 5 == 0) b /= 5
	b == 1
}


def divisible(num: Int): Boolean = {
	var cur = 0
	var set = collection.mutable.Set.empty[Int]

	var i = 1
	while (true) {
		cur = (cur * 10 + 1) % num
		if (cur == 0) {
			return isTen(i)
		} else if (set(cur)) {
			return false
		}
		set += cur
		i += 1	
	}
	false
}


val result = helpers.Sieve(100000).primesIter().filterNot(divisible).sum

println(result)
