package aoc2019

import aoc2019.parser.Parser
import aoc2019.parser.Parser._

import scala.collection.mutable

object day3 {

  case class Visit(location: Vec2, steps: Int)

  case class WirePath(direction: Direction, distance: Int)

  object Solution extends Solution[(Seq[WirePath], Seq[WirePath])] {
    def part1: Int = {
      val locations1 = visitedLocations(input._1).map(_.location)
      val locations2 = visitedLocations(input._2).map(_.location)

      val intersections = locations1 & locations2

      intersections.toSeq.map { case Vec2(x, y) => x + y }.min
    }

    def part2: Int = {
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

  def visitedLocations(paths: Seq[WirePath]): Set[Visit] = {
    val locations = mutable.Set[Visit]()
    var location = Vec2(0, 0)
    var steps = 0

    for (path <- paths) {
      for (_ <- 1 to path.distance) {
        location = path.direction.shift(location)
        steps += 1
        locations.add(Visit(location, steps))
      }
    }

    locations.toSet
  }

  val wirePath: Parser[Seq[WirePath]] = (Direction.parser ~ int).map(WirePath.tupled).separatedBy(",")

  implicit val parser: Parser[(Seq[WirePath], Seq[WirePath])] = (wirePath <~ "\n") ~ wirePath
}
