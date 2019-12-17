package aoc2019.day14

import aoc2019.Solution

object Part2 extends Solution[Reaction] {

  override protected def solution: Long = {
    binarySearch(1000000000000L, Reaction.oreForFuel(input, _), 1, Int.MaxValue)
  }

  private def binarySearch(expected: Long, op: Long => Long, min: Long = Long.MinValue, max: Long = Long.MaxValue): Long = {
    val value = (max + min) / 2
    val result = op(value)

    if (result == expected || value == min) {
      value
    } else if (result > expected) {
      binarySearch(expected, op, min, value)
    } else {
      binarySearch(expected, op, value, max)
    }
  }
}
