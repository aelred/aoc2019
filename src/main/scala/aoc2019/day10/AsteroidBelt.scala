package aoc2019.day10

import aoc2019.Vec2

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
