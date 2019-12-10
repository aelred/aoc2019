package aoc2019.day10

import aoc2019.FromString

case class Asteroids(asteroids: Seq[Boolean])

object Asteroids {
  implicit val fromString: FromString[Asteroids] = str => Asteroids(str.toSeq.map(_ == '#'))
}
