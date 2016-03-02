val sieve = helpers.Sieve(1000)

val result = Iterator.from(600).map( i =>
	i -> (sieve.factorsOf(i).distinct.size >= 4)
).sliding(4).find(
	_.forall(_._2)
).get.head._1

println(result)
