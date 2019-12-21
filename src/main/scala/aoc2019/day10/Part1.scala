package aoc2019.day10

import aoc2019.Solution

object Part1 extends Solution[Seq[Asteroids]] {
  def solution: Int = {
    val belt = AsteroidBelt(input)

    belt.asteroids.map(belt.visibleAsteroids(_).size).max
  }
}
