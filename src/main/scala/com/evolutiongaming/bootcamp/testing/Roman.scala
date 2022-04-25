package com.evolutiongaming.bootcamp.testing

object Roman {

  private val romanToDecimal = Map(
    "I" -> 1,
    "V" -> 5,
    "X" -> 10,
    "L" -> 50,
    "C" -> 100,
    "D" -> 500,
    "M" -> 1000,
    "IV" -> 4,
    "IX" -> 9,
    "XL" -> 40,
    "XC" -> 90,
    "CD" -> 400,
    "CM" -> 900
  )

  private val romanToDecimalDescending = romanToDecimal.toList.sortBy(_._2)(Ordering[Int].reverse)

  def roman(n: Int): String =
    if (n == 0) ""
    else {
      romanToDecimalDescending.find(n >= _._2) match {
        case Some((r, d)) => r + roman(n - d)
        case None => throw new IllegalArgumentException
      }
    }

  def decimal(s: String): Option[Int] =
    if (s.isEmpty) Some(0)
    else {
      val rLength = if (s.length >= 2 && romanToDecimal.contains(s.substring(0, 2))) 2 else 1
      val r = s.substring(0, rLength)
      romanToDecimal.get(r) match {
        case Some(d) => decimal(s.substring(rLength)).map(d + _)
        case None => None
      }
    }
}
