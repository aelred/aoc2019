package aoc2019.day12

import aoc2019.{Vec3, Solution}

object Part1 extends Solution[Vec3] {
  def solution: Int = {
    val initialPlanets = input map {
      Planet(_, Vec3(0, 0, 0))
    }

    val planets = (1 to 1000).foldLeft(initialPlanets)((ps, _) => Planet.simulate(ps))

    planets.map(_.energy).sum
  }
}
