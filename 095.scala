val N = 1000000
val sieve = helpers.Sieve(1000)

var seen = Set(1)
var longest = -1
var result = 0
(1 to N).foreach { param =>
	var i = param
	var step = 0
	var chain = Map.empty[Int, Int]
	while (i < N && !seen(i) && !chain.contains(i)) {
		seen += i
		step += 1
		chain += i -> step
		i = sieve.sumOfDivisors(i).toInt - i
	}
	if (chain.contains(i)) {
		val length = step - chain(i) + 1
		if (length > longest) {
			longest = length
			result = chain.filter(_._2 >= chain(i)).keys.min
		}
	}
}


println(result)
