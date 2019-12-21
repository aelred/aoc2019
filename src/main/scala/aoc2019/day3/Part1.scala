package aoc2019.day3

import aoc2019.Vec2

object Part1 extends WireSolution {

  def solution: Int = {
    val locations1 = visitedLocations(input._1).map(_.location)
    val locations2 = visitedLocations(input._2).map(_.location)

    val intersections = locations1 & locations2

    intersections.toSeq.map{case Vec2(x, y) => x + y}.min
  }
}
