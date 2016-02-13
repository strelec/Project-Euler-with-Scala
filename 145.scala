val result = (1 until 1000000000).count( i =>
	i % 10 != 0 &&
	(i + i.toString.reverse.toInt).toString.forall( c =>
		c == '1' || c == '3' || c == '5' || c == '7' || c == '9'
	)
)

println(result)