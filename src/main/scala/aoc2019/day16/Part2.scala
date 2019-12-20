package aoc2019.day16

import aoc2019.Solution

object Part2 extends Solution[Seq[Int]] {
  def solution: String = {
    val signal = Seq.fill(10000)(line).flatten
    val offset = signal.take(7).mkString.toInt
    val shortSignal = signal.drop(offset)
    LazyList.iterate(shortSignal)(shortPhase)(100).take(8).mkString
  }

  def shortPhase(signal: Seq[Int]): Seq[Int] = signal.scanRight(0)(_ + _).dropRight(1).map(_ % 10)
}
