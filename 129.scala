val N = 1000000

def A(n: Int): Int =
	if (n % 2 == 0 || n % 5 == 0) 0
	else {
		var cur = 0
		var i = 0
		do {
			cur = (cur * 10 + 1) % n
			i += 1
		} while (cur != 0)
		i
	}

val result = Iterator.from(N).find(A(_) > N).get
println(result)
