package aoc2019

import aoc2019.parser.Parser

package object day16 {

  implicit val parser: Parser[Seq[Int]] = Parser.digit.repeat

  private val Pattern = LazyList.continually(Seq(0, 1, 0, -1)).flatten

  def fft(signal: Seq[Int]): LazyList[Seq[Int]] = LazyList.iterate(signal)(phase)

  def phase(signal: Seq[Int]): Seq[Int] = {

    def calcDigit(digit: Int) = {
      val stretchedPattern = Pattern.flatMap(Seq.fill(digit)(_)).drop(1)
      val pairs = signal zip stretchedPattern
      val result = pairs.map { case (x, y) => x * y }.sum
      result.abs % 10
    }

    (1 to signal.length).map(calcDigit)
  }
}
