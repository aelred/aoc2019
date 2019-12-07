package aoc2019.day6

import aoc2019.Solution

object Part2 extends Solution[Orbit] {

  override def solution: Int = {
    val orbitMap = input.map(orbit => orbit.satellite -> orbit.planet).toMap

    def pathToRoot(obj: String): Seq[String] =
      orbitMap.get(obj).map(pathToRoot).getOrElse(Seq.empty) :+ obj

    val youToRoot = pathToRoot(orbitMap("YOU")).toSet
    val sanToRoot = pathToRoot(orbitMap("SAN")).toSet

    ((youToRoot diff sanToRoot) | (sanToRoot diff youToRoot)).size
  }

}
