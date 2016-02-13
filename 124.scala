val table = helpers.MultiFactoring(100000).table

val result = (1 to 100000).map(x => table(x).product -> x).sorted

println(result(9999))
