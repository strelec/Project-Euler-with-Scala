import helpers._

val result = Stream.from(600).map( i =>
	i -> (NativeFactoring(i).factor.size >= 4)
).sliding(4).find(
	_.forall(_._2)
).get.head._1

println(result)