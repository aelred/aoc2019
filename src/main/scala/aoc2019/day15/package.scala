package aoc2019

import aoc2019.program.Program

import scala.collection.mutable

package object day15 {

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

      for (direction <- Direction.all) {
        if (!map.contains(direction.shift(pos))) {
          unexplored.add(pos)
        }
      }

      directions.enqueue(nextDir)

      print("\u001B[0;0H")
      val drawMap = map.view.mapValues {
        case Wall() => '█'
        case Clear() => '.'
        case Oxygen() => 'O'
      }.toMap
      val drawDrone = Map(pos -> 'X')
      println(aoc2019.mapToString((drawMap ++ drawDrone).view))
    }

    map.toMap
  }
}
