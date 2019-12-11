package aoc2019.day3

import aoc2019.Pos

object Part1 extends WireSolution {

  def solution: Int = {
    val locations1 = visitedLocations(input(0)).map(_.location)
    val locations2 = visitedLocations(input(1)).map(_.location)

    val intersections = locations1 & locations2

    intersections.toSeq.map{case Pos(x, y) => x + y}.min
  }
}
