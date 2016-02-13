import helpers._

val seq = Vector(1,3,7,9,13,27).reverse.map(_.toLong)

val ns = for {
	p <- Seq(10L, 20L)
	n <- p until 15000000L by 30L

	if seq.forall{ i =>
		println(s"FAKTOR $n² + $i")
		SysFactoring(n*n + i).isPrime
	}
} yield n

println(ns.sum)