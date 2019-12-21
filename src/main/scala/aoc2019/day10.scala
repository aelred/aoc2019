package aoc2019

import aoc2019.parser.Parser
import aoc2019.parser.Parser._

import scala.collection.mutable

object day10 {

  case class Asteroids(asteroids: Seq[Boolean])

  object Asteroids {
    implicit val parser: Parser[Asteroids] =
      ("#" >> (_ => true) | "." >> (_ => false)).repeat >> (Asteroids(_))
  }

  class AsteroidBelt(val asteroids: Set[Vec2]) {

    def canSee(pos1: Vec2, pos2: Vec2): Boolean = {
      val diff = pos2 - pos1

      val range = 1 to diff.x.abs.max(diff.y.abs)

      val stepFactor = range.filter { num =>
        diff.x % num == 0 && diff.y % num == 0
      }.max

      val step = diff / stepFactor

      var pos = pos1 + step

      while (pos != pos2) {
        if (asteroids.contains(pos)) return false

        pos += step
      }

      true
    }

    def visibleAsteroids(pos: Vec2): Set[Vec2] = {
      asteroids filter { _ != pos } filter { canSee(pos, _) }
    }

    def without(toRemove: Set[Vec2]): AsteroidBelt = new AsteroidBelt(asteroids &~ toRemove)
  }

  object AsteroidBelt {
    def apply(asteroids: Seq[Asteroids]) = {
      val table = asteroids.map(_.asteroids)

      val positions = for {
        (row, y) <- table.zipWithIndex
        (cell, x) <- row.zipWithIndex
        if cell
      } yield Vec2(x, y)

      new AsteroidBelt(positions.toSet)
    }
  }

  object Solution extends Solution[Seq[Asteroids]] {

    def part1: Int = {
      val belt = AsteroidBelt(input)

      belt.asteroids.map(belt.visibleAsteroids(_).size).max
    }

    def part2: Int = {
      var belt = AsteroidBelt(input)
      val asteroidsDestroyed = mutable.Buffer[Vec2]()

      val station = belt.asteroids.maxBy(belt.visibleAsteroids(_).size)

      while (belt.asteroids.size > 1) {
        val visibleAsteroids = belt.visibleAsteroids(station)

        belt = belt.without(visibleAsteroids)

        asteroidsDestroyed ++= visibleAsteroids.toSeq.sortBy(asteroid => (asteroid - station).angle)
      }

      val asteroid = asteroidsDestroyed(199)

      asteroid.x * 100 + asteroid.y
    }
  }
}
