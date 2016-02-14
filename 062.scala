def digits(n: Long) = {
	val str = n.toString.map(_.asDigit)
	Seq.tabulate(10)( i =>
		str.count(_ == i)
	)
} 

val result = for {
	i <- Stream.from(3)
	
	start = math.cbrt(math.pow(10, i-1)).toLong
	stop = math.cbrt(math.pow(10, i)).toLong - 1
	cubes = (start to stop).map(x => x*x*x)
	
	grp = cubes.groupBy(digits).toVector
	found <- grp.filter(_._2.size == 5).sortBy(_._2.head)
} yield found._2

println(result.head.head)
