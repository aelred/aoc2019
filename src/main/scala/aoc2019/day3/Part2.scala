package aoc2019.day3

object Part2 extends WireSolution {

  def solution: Int = {
    val visits1 = visitedLocations(input._1)
    val visitMap1 = visits1.map(v => v.location -> v.steps).toMap
    val visits2 = visitedLocations(input._2)
    val visitMap2 = visits2.map(v => v.location -> v.steps).toMap

    val locations1 = visits1.map(_.location)
    val locations2 = visits2.map(_.location)

    val intersections = locations1 & locations2

    val combinedSteps = intersections map { location => visitMap1(location) + visitMap2(location) }

    combinedSteps.min
  }
}
