package aoc2019

import aoc2019.program.Program

trait FromString[T] {
  def apply(string: String): T

  def map[S](f: T => S): FromString[S] = string => f(this(string))
}

object FromString {
  implicit val int: FromString[Int] = _.toInt

  implicit val range: FromString[Range] = { string =>
    val nums = string.split('-').map(int(_))
    nums(0) to nums(1)
  }

  implicit def csv[T: FromString]: FromString[Seq[T]] = string => {
    val fromString = implicitly[FromString[T]]
    string.split(',').toSeq.map(fromString(_))
  }

  implicit val program: FromString[Program] = csv[Int].map(new Program(_))
}
