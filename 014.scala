val memo = collection.mutable.Map(1L -> 0)

def get(i: Long): Int = memo.get(i) match {
	case Some(n) => n
	case None =>
		val v = get(if (i % 2 == 0) i/2 else 3*i+1) + 1
		memo(i) = v
		v
}

val result = (1L until 1000000L).maxBy(get)

println(result)