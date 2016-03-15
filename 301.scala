val N = 1 << 30

val result = (1 to N).count( i =>
	(i ^ (2*i) ^ (3*i)) == 0
)
println(result)
