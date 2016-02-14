def daysInMonth(year: Int, month: Int) = {
	val leap = year % 4 == 0 && year % 100 != 0 || year % 400 == 0
	val data = Vector(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
	if (month == 1 && leap) 29 else data(month)
}

var count = 0
var days = 0
for {
	year <- 1901 to 2000
	month <- 0 until 12
} {
	if (days % 7 == 6)
		count += 1
	days += daysInMonth(year, month)
}

println(count)