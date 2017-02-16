val D = 1

val p10 = 1L +: helpers.Number.powers(10).take(15).toVector
val c10 = (0L +: p10).zipWithIndex.map { case (p, i) =>
	p * i
}

def log10(num: Long): Int = {
	var n = num
	var result = 0
	while (n >= 10) {
		n /= 10
		result += 1
	}
	result
}

var sum = 0L

for (D <- 1 to 9) {
	def f(n: Long, log: Int): Long = if (n == 0) 0 else {
		val p = p10(log)
		val leading = n / p
		val rest = n % p
		leading * c10(log) + f(rest, log - 1) +
		(if (D < leading) p else if (D == leading) rest + 1 else 0)
	}

	def g(n: Long): Long = {
		var (low, high) = (0L, 100000000000L)
		while (low <= high) {
			val mid = (low + high) / 2
			if (f(mid, log10(mid)) > n) high = mid - 1
			else low = mid + 1
		}
		high
	}

	var n = 1000000000000L
	while (n > 0) {
		val fn = f(n, log10(n))
		if (fn == n) {
			sum += n
			n -= 1
		} else if (fn < n) {
			n = fn
		} else {
			val gn = g(n)
			if (gn < n) n = gn else n -= 1
		}
	}
}

println(sum)
