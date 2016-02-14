import helpers._

val result = (23 to 100).map( n =>
	n - 2*Helpers.binoms(n).takeWhile(_ < 1000000).size
)

println(result.sum)
