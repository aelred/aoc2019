package aoc2019

import aoc2019.parser.Parser
import aoc2019.parser.Parser._

object day6 {

  private val name = "...".r
  implicit val parser: Parser[Orbit] = (name <~ ")") ~ name >> Orbit.tupled

  case class Orbit(planet: String, satellite: String)

  object Solution extends Solution[Seq[Orbit]] {

    def part1: Int = {
      val orbitMap = input.map(orbit => orbit.satellite -> orbit.planet).toMap

      def distanceFromRoot(obj: String): Int =
        orbitMap.get(obj).map(distanceFromRoot(_) + 1).getOrElse(0)

      orbitMap.keys.toSeq.map(distanceFromRoot).sum
    }

    def part2: Int = {
      val orbitMap = input.map(orbit => orbit.satellite -> orbit.planet).toMap

      def pathToRoot(obj: String): Seq[String] =
        orbitMap.get(obj).map(pathToRoot).getOrElse(Seq.empty) :+ obj

      val youToRoot = pathToRoot(orbitMap("YOU")).toSet
      val sanToRoot = pathToRoot(orbitMap("SAN")).toSet

      ((youToRoot diff sanToRoot) | (sanToRoot diff youToRoot)).size
    }

  }

}
