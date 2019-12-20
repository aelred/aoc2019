package aoc2019.day16

import aoc2019.Solution

object Part1 extends Solution[Seq[Int]] {
  def solution: String = fft(line)(100).take(8).mkString
}
