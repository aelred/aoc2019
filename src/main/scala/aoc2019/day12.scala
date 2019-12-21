package aoc2019

object day12 extends Solution[Seq[Vec3]] {

  def part1: Int = {
    val initialPlanets = input map {
      Planet(_, Vec3(0, 0, 0))
    }

    val planets = (1 to 1000).foldLeft(initialPlanets)((ps, _) => Planet.simulate(ps))

    planets.map(_.energy).sum
  }

  def part2: Long = {
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

  case class Planet(position: Vec3, velocity: Vec3) {

    def energy: Int = {
      val potential = Math.abs(position.x) + Math.abs(position.y) + Math.abs(position.z)
      val kinetic = Math.abs(velocity.x) + Math.abs(velocity.y) + Math.abs(velocity.z)

      potential * kinetic
    }

    def attractTo(planet: Planet): Planet = {
      val diff = (planet.position - position).map(_.compare(0))
      copy(velocity=velocity + diff)
    }

    def move: Planet = copy(position=position + velocity)
  }

  object Planet {
    def simulate(planets: Seq[Planet]): Seq[Planet] = {
      val afterGravity = planets.map(planet => planets.foldLeft(planet)(_.attractTo(_)))
      afterGravity.map(_.move)
    }
  }
}
