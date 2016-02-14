val N = 1000000

val fac = helpers.MultiFactoring(N)
val result = (2 to N).map(fac.totient).map(_.toLong).sum

println(result)
