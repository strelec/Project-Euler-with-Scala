def compute: Set[Long] = {
	var set = Set.empty[Long]

	var b = 2L
	while(true) {
		var power = b * b * b
		var cur = power + b*b + b + 1
		var break = true

		while (cur < 1000000000000L) {
			set += cur

			power *= b
			cur += power
			break = false
		}

		b += 1
		if (break)
			return set
	}
	return set
}

// sum of all repunits 111 for all bases
// sum b*b + b + 1 for 2 to 999999
val sum = 333333333333999996L

// is already included in the above sum
def alreadyIncluded(n: Long) = {
	val b = ((math.sqrt(4*n-3) - 1) / 2).round
	b*b + b + 1 == n
}

println(1 + sum + compute.filterNot(alreadyIncluded).sum)