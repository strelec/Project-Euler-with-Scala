type Coordinate = (Int, Int) //Longitude, Latitude

def convexHull(coords: List[Coordinate]) = {
	//returns > 0 if points form a counter clockwise turn,
	// < 0 if clockwise, and 0 if collinear
	def ccw(p1: Coordinate, p2: Coordinate, p3: Coordinate) =
		(p2._1 - p1._1)*(p3._2 - p1._2) - (p2._2 - p1._2)*(p3._1 - p1._1)

	//Scan the List of coordinates and find our vertices
	def scan(theCoords: List[Coordinate]): List[Coordinate] = theCoords match {
		case xs if xs.size <= 2 => xs
		case x::y::z::xs if ccw(x,y,z) > 0 => x::scan(y::z::xs)
		case x::y::z::xs => scan(x::z::xs)
	}

	//find the coordinate with the lowest latitude
	val origin = coords.minBy(_._2)
	//sort the rest of the points according to their polar angle (the angle between
	//the line defined by the origin and the current point, and the x-axis)
	val coordList = origin :: coords.filter(_ != origin).sortBy( point =>
		math.atan2(point._2 - origin._2, point._1 - origin._1)
	)

	scan(coordList)
}

def area(coords: List[Coordinate]) =
	(coords.last :: coords).sliding(2).map {
		case List((ax,ay), (bx,by)) => (ax - bx) * (ay + by) / 2.0
	}.sum

val N = 10

val result = (0 until 10).map { _ =>
	val points = for {
		i <- (0 to N).toList
		j <- 0 to N
		if util.Random.nextInt(N+1) == 0
	} yield (i, j)

if (points.nonEmpty) {
	println(points)
	println(convexHull(points))
	println(area(convexHull(points)))
}
	if (points.isEmpty) 0.0 else area(convexHull(points))
}.sum / 100000

println(result)
