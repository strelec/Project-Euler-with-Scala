import scala.collection.Searching._
import helpers._

val abundant = (1 to 28123).filter( i =>
	NativeFactoring(i).divisorSum > 2*i
)

val result = (1 to 28123).filter( i =>
	abundant.takeWhile(_ < i).forall( j =>
		abundant.search(i - j) match {
			case Found(_) => false
			case _ => true
		}
	)
)

println(result.sum)
