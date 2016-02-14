val apoens = Vector(1, 2, 5, 10, 20, 50, 100, 200)

var memo = collection.mutable.Map.empty[(Int, Int), Int]

def count(rem: Int, cur: Int): Int =
	if (cur == 0)
		1 
	else memo.getOrElse((rem, cur), {
		val apoen = apoens(cur)
		val ret = (0 to rem/apoen).map( i =>
			count(rem - i*apoen, cur - 1)
		).sum
		memo((rem, cur)) = ret
		ret
	})

println(count(200, 7))
