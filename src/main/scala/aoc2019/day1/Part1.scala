package aoc2019.day1

import aoc2019.Solution

object Part1 extends Solution[Seq[Int]] {
  def solution: Int = input
      .map(x => (x / 3) - 2)
      .sum
}
