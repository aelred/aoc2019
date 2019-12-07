package aoc2019.day6

import aoc2019.FromString

case class Orbit(planet: String, satellite: String)

object Orbit {
  implicit def fromString: FromString[Orbit] = { string =>
    val split = string.split(')')
    Orbit(split(0), split(1))
  }
}
