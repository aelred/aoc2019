package aoc2019

import aoc2019.parser.Parser
import aoc2019.parser.+
import aoc2019.parser.Parser.{int, lit}

case class Vec3(x: Int, y: Int, z: Int) {
  def +(vec: Vec3): Vec3 = Vec3(x + vec.x, y + vec.y, z + vec.z)
  def -(vec: Vec3): Vec3 = Vec3(x - vec.x, y - vec.y, z - vec.z)
  def *(value: Int): Vec3 = map(_ * value)
  def /(value: Int): Vec3 = map(_ / value)

  def map(f: Int => Int): Vec3 = Vec3(f(x), f(y), f(z))
}

object Vec3 {
  implicit val parser: Parser[Vec3] =
    (lit("<x=") + int + lit(", y=") + int + lit(", z=") + int + lit(">")) map {
      case _ + x + _ + y + _ + z + _ => Vec3(x, y, z)
    }
}