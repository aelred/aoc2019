package aoc2019

import aoc2019.parser.Parser

object day16 {

  implicit val parser: Parser[Seq[Int]] = Parser.digit.repeat

  object Solution extends Solution[Seq[Int]] {

    def part1: String = fft(input)(100).take(8).mkString

    def part2: String = {
      val signal = Seq.fill(10000)(input).flatten
      val offset = signal.take(7).mkString.toInt
      val shortSignal = signal.drop(offset)
      LazyList.iterate(shortSignal)(shortPhase)(100).take(8).mkString
    }
  }

  private val Pattern = LazyList.continually(Seq(0, 1, 0, -1)).flatten

  def shortPhase(signal: Seq[Int]): Seq[Int] = signal.scanRight(0)(_ + _).dropRight(1).map(_ % 10)

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
