val result = (BigInt(1) to BigInt(100)).product.toString.map(_.asDigit).sum

println(result)
