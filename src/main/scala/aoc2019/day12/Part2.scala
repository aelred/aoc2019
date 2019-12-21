package aoc2019.day12

import aoc2019.{Solution, Vec3}

object Part2 extends Solution[Seq[Vec3]] {
  def solution: Long = {
    val initialPlanets = input map {
      Planet(_, Vec3(0, 0, 0))
    }

    var planets = initialPlanets
    var iter = 0L
    var xPeriod: Option[Long] = None
    var yPeriod: Option[Long] = None
    var zPeriod: Option[Long] = None

    def comparePlanets(f: Vec3 => Int): Boolean = {
      for ((p1, p2) <- initialPlanets.zip(planets)) {
        if (f(p1.position) != f(p2.position) || f(p1.velocity) != f(p2.velocity)) {
          return false
        }
      }

      true
    }

    while (xPeriod.isEmpty || yPeriod.isEmpty || zPeriod.isEmpty) {
      planets = Planet.simulate(planets)
      iter += 1

      if (xPeriod.isEmpty && comparePlanets(_.x)) {
        log(s"x: $iter")
        xPeriod = Some(iter)
      }
      if (yPeriod.isEmpty && comparePlanets(_.y)) {
        log(s"y: $iter")
        yPeriod = Some(iter)
      }
      if (zPeriod.isEmpty && comparePlanets(_.z)) {
        log(s"z: $iter")
        zPeriod = Some(iter)
      }
    }

    val xp = BigInt(xPeriod.get)
    val yp = BigInt(yPeriod.get)
    val zp = BigInt(zPeriod.get)

    val lcmXY = (xp * yp).abs / xp.gcd(yp)

    val lcmXYZ = (lcmXY * zp).abs / lcmXY.gcd(zp)

    lcmXYZ.toLong
  }
}
