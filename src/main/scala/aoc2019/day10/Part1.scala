package aoc2019.day10

import aoc2019.Solution

object Part1 extends Solution[Asteroids] {
  def solution: Int = {
    val belt = AsteroidBelt(input)

    belt.asteroids.map {asteroid =>
      belt.asteroids filter { _ != asteroid } count { belt.canSee(asteroid, _) }
    }.max
  }
}
