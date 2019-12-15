package aoc2019.day12

import aoc2019.Vec3

import scala.collection.mutable

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
