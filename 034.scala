val fact = (0 to 9).map(x => x -> (1 to x).product).toMap

val result = (10 to 2540160).filter( i=>
	i == i.toString.map(_.asDigit).map(fact).sum
)

println(result.sum)