val memo = collection.mutable.Map(1 -> false, 89 -> true)

def get(i: Int): Boolean = memo.get(i) match {
	case Some(n) => n
	case None =>
		val v = get(i.toString.map(x => x.asDigit * x.asDigit).sum)
		memo(i) = v
		v
}

val result = (1 until 10000000).count(get)

println(result)