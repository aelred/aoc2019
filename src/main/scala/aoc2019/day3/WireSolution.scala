package aoc2019.day3

import aoc2019._
import aoc2019.parser.Parser
import aoc2019.parser.Parser.{int, lit}
import WireSolution.parser

import scala.collection.mutable

abstract class WireSolution extends Solution[Seq[WirePath]] {

  protected def visitedLocations(paths: Seq[WirePath]): Set[Visit] = {
    val locations = mutable.Set[Visit]()
    var location = Pos(0, 0)
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

  val direction: Parser[Direction] =
    lit("U").map(_ => Up   ()) |
    lit("D").map(_ => Down ()) |
    lit("L").map(_ => Left ()) |
    lit("R").map(_ => Right())

  implicit val parser: Parser[Seq[WirePath]] = (direction + int).map(WirePath.tupled).split(lit(","))
}
