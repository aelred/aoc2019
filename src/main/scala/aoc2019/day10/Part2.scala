package aoc2019.day10

import aoc2019.Solution

import scala.collection.mutable

object Part2 extends Solution[Asteroids] {
  def solution: Int = {
    var belt = AsteroidBelt(input)
    val asteroidsDestroyed = mutable.Buffer[Pos]()

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
