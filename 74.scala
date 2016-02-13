val fact = (1 to 9).inits.map(_.product).toVector.reverse
def next(num: Int) = {
	var n = num
	var sum = 0
	while(n != 0) {
		sum += fact(n % 10)
		n /= 10
	}
	sum
}

val N = 1000000 - 1
val result = (1 to N).count { num =>
	var n = num
	var set = Set.empty[Int]
	while(!set(n) && set.size <= 61) {
		set += n
		n = next(n)
	}
	set.size == 60
}

println(result)
