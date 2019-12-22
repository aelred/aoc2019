package aoc2019

import aoc2019.program.Program

import scala.collection.mutable

object day15 extends Solution[Program] {

  def part1: Int = {
    val map = explore(input)

    val oxygen = map.find { case (_, cell) => cell == Oxygen() }.get._1

    val nonWalls: Set[Vec2] = map.filter { case (_, cell) => cell != Wall() }.map { case (p, _) => p }.toSet

    val neighbours: Map[Vec2, Set[Vec2]] = nonWalls.map { pos => pos -> pos.neighbours }.toMap

    dijkstra(nonWalls, neighbours.apply, Vec2(0, 0))(oxygen)
  }

  def part2: Int = {
    val map = explore(input)

    val oxygen = map.find { case (_, cell) => cell == Oxygen() }.get._1

    val nonWalls: Set[Vec2] = map.filter { case (_, cell) => cell != Wall() }.map { case (p, _) => p }.toSet

    val neighbours: Map[Vec2, Set[Vec2]] = nonWalls.map { pos => pos -> pos.neighbours }.toMap

    dijkstra(nonWalls, neighbours.apply, oxygen).values.max - 1
  }

  sealed trait Cell
  case class Wall() extends Cell
  case class Clear() extends Cell
  case class Oxygen() extends Cell

  def dijkstra[T](nodes: Set[T], neighbours: T => Set[T], start: T): Map[T, Int] = {
    val unvisited = mutable.Set.from(nodes)

    val distance = mutable.Map[T, Int]().withDefaultValue(Int.MaxValue)
    distance(start) = 0

    while (unvisited.nonEmpty) {
      val current = unvisited.minBy(distance)

      for (neighbour <- neighbours(current)) {
        distance(neighbour) = distance(current) + 1.min(distance(neighbour))
      }

      unvisited.remove(current)
    }

    distance.toMap
  }

  def explore(program: Program): Map[Vec2, Cell] = {
    val directions = mutable.Queue[Direction](Right())
    var pos = Vec2(0, 0)

    val map = mutable.Map[Vec2, Cell]()
    val unexplored = mutable.Set[Vec2]()
    unexplored.add(pos)

    def input() = directions.dequeue() match {
      case Up() => 1
      case Down() => 2
      case Left() => 3
      case Right() => 4
    }

    program.runWhile(() => unexplored.nonEmpty)(input) { next =>
      val thisDir = directions.front
      val checkedPos = thisDir.shift(pos)

      val output = next()
      val cell = output match {
        case 0 => Wall()
        case 1 => Clear()
        case 2 => Oxygen()
      }

      val (nextPos, nextDir) = cell match {
        case Wall() =>
          (pos, thisDir.clockwise)
        case _ =>
          (checkedPos, thisDir.anticlockwise)
      }
      map(checkedPos) = cell
      unexplored.remove(checkedPos)

      pos = nextPos

      for (neighbour <- pos.neighbours) {
        if (!map.contains(neighbour)) {
          unexplored.add(pos)
        }
      }

      directions.enqueue(nextDir)

      logRaw("\u001B[0;0H")
      val drawMap = map.view.mapValues {
        case Wall() => '█'
        case Clear() => '.'
        case Oxygen() => 'O'
      }.toMap
      val drawDrone = Map(pos -> 'X')
      log(aoc2019.mapToString((drawMap ++ drawDrone).view))
    }

    map.toMap
  }
}
