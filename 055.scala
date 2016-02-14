val result = (1 until 10000).filter { num =>
	var n = BigInt(num + num.toString.reverse.toInt)
	var isLychrel = true
	(1 to 50).foreach { _ =>
		val s = n.toString
		val sr = s.reverse
		n += BigInt(sr)
		
		if (s == sr)
			isLychrel = false
	}
	isLychrel
}

println(result.size)
