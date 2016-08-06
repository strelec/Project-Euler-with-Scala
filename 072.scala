val N = 1000000

val totients = sieves.Totients(N)
val result = (2 to N).map(totients(_).toLong).sum

println(result)
