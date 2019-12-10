package aoc2019.day10

class AsteroidBelt(val asteroids: Set[Pos]) {

  def canSee(pos1: Pos, pos2: Pos): Boolean = {
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
}

object AsteroidBelt {
  def apply(asteroids: Seq[Asteroids]) = {
    val table = asteroids.map(_.asteroids)

    val positions = for {
      (row, y) <- table.zipWithIndex
      (cell, x) <- row.zipWithIndex
      if cell
    } yield Pos(x, y)

    new AsteroidBelt(positions.toSet)
  }
}
