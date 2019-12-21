package aoc2019.day3

import aoc2019._
import aoc2019.day3.WireSolution.parser
import aoc2019.parser.Parser
import aoc2019.parser.Parser.{Literal, int}

import scala.collection.mutable

abstract class WireSolution extends Solution[(Seq[WirePath], Seq[WirePath])] {

  protected def visitedLocations(paths: Seq[WirePath]): Set[Visit] = {
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
}

object WireSolution {

  val direction: Parser[Direction] =
    "U" >> { _ => Up   ()} |
    "D" >> { _ => Down ()} |
    "L" >> { _ => Left ()} |
    "R" >> { _ => Right()}

  val wirePath: Parser[Seq[WirePath]] = (direction ~ int).map(WirePath.tupled).separatedBy(",")
  implicit val parser: Parser[(Seq[WirePath], Seq[WirePath])] = (wirePath <~ "\n") ~ wirePath
}
