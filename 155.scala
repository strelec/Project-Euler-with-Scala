var prev = Set((1, 1))
var cumul = prev

(2 to 18).foreach { _ =>
	prev = prev.flatMap { case (n, d) =>
		Seq((n+d, d), (d, n+d))
	}.map { case (n, d) =>
		val c = helpers.Helpers.gcd(n, d)
		(n/c, d/c)
	}
	cumul |= prev
}

println(cumul.size)
