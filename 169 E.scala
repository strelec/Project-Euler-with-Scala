/*
	EXPLANATION:
	
	This task is actually realy simple to do with dynamic programming and a simple observation.
	
	1. Recursive function gets called with number and the highest power remaining.
	2. In the function, we try to subtract higest power either two, one or zero times and recurse with highest power haved.
	3. It is really important to bound the branching by observing once the number gets sufficently above the highest power, you can just say there are no partitions, therefore return zero.
*/

val N = BigInt(10).pow(25)

def msb(j: BigInt) = {
	var i = BigInt(2)
	while(i < j)
		i *= 2
	i / 2
}

var memo = collection.mutable.Map.empty[(BigInt, BigInt), Long]

def count(rem: BigInt, cur: BigInt): Long =
	if (rem > 4*cur - 2) 0
	else if (cur == 1) 1
	else memo.getOrElse((rem, cur), {
		var ret = count(rem, cur/2)
		if (rem >= cur)
			ret += count(rem - cur, cur/2)
		if (rem >= 2*cur)
			ret += count(rem - 2*cur, cur/2)
		memo(rem -> cur) = ret
		ret
	})

println(count(N, msb(N)))
