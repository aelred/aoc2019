package aoc2019.day3

import aoc2019.{FromString, Solution}

import scala.collection.mutable

abstract class WireSolution extends Solution[Seq[WirePath]]()(FromString.csv(WireSolution.fromString(FromString.int))) {

  protected def visitedLocations(paths: Seq[WirePath]): Set[Visit] = {
    val locations = mutable.Set[Visit]()
    var location = (0, 0)
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
}

object WireSolution {

  implicit def fromString(implicit parseInt: FromString[Int]): FromString[WirePath] = value => {
    val (dirStr, distStr) = value.splitAt(1)

    val direction = dirStr match {
      case "U" => Up()
      case "D" => Down()
      case "L" => Left()
      case "R" => Right()
    }

    val distance = parseInt(distStr)

    WirePath(direction, distance)
  }
}
