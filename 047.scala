val sieve = sieves.FactorCount(150000)

val result = Iterator.from(1).map( i =>
	i -> (sieve(i) >= 4)
).sliding(4).find(
	_.forall(_._2)
).get.head._1

println(result)
