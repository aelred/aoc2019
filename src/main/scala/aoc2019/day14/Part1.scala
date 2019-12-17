package aoc2019.day14

import aoc2019.Solution

object Part1 extends Solution[Reaction] {

  override protected def solution: Long = {
    Reaction.oreForFuel(input, 1)
  }
}
