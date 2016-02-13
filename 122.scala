var set = Map(1 -> 0)

while(set.size < 200) {
	set = (for {
		(a, am) <- set
		(b, bm) <- set
		c = a + b
		if c <= 200
		cm = math.max(am, bm) + 1
	} yield c -> cm) ++ set
	println(set)
}

println(set.values.sum)

