package aoc2019.day1

import aoc2019.Solution

object Part1 extends Solution[Int] {
  def solution: Int = input
      .map(x => (x / 3) - 2)
      .sum
}
