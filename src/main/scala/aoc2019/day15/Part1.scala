package aoc2019.day15

import aoc2019._
import aoc2019.program.Program

object Part1 extends BaseSolution {

  override protected def solution: Int = {
    val map = explore(input)

    val oxygen = map.find { case (_, cell) => cell == Oxygen() }.get._1

    val nonWalls: Set[Vec2] = map.filter { case (_, cell) => cell != Wall() }.map { case (p, _) => p }.toSet

    val neighbours: Map[Vec2, Set[Vec2]] = nonWalls.map { pos =>
      pos -> Direction.all.map(_.shift(pos))
    }.toMap

    dijkstra(nonWalls, neighbours.apply, Vec2(0, 0))(oxygen)
  }
}
