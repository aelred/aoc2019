package aoc2019.day6

import aoc2019.Solution

object Part1 extends Solution[Orbit] {

  override def solution: Int = {
    val orbitMap = input.map(orbit => orbit.satellite -> orbit.planet).toMap

    def distanceFromRoot(obj: String): Int =
      orbitMap.get(obj).map(distanceFromRoot(_) + 1).getOrElse(0)

    orbitMap.keys.toSeq.map(distanceFromRoot).sum
  }

}
