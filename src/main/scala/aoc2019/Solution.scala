package aoc2019

import scala.io.Source

abstract class Solution[T](implicit fromString: FromString[T]) {

  protected final var input: Seq[T] = _
  protected final def line: T = input(0)

  protected def solution: Any

  final def main(args: Array[String]): Unit = {
    val path = getClass.getPackage.getName.replaceAllLiterally(".", "/")
    val name = s"$path.txt"
    input = Source.fromResource(name).getLines.map(fromString(_)).toSeq
    print(solution)
  }
}
