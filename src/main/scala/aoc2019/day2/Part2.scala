package aoc2019.day2

import aoc2019.Solution
import aoc2019.program.Program

object Part2 extends Solution[Program] {

  def solution: Int = {
    val program = input

    val matchingParams = for {
      noun <- 0 to 100
      verb <- 0 to 100
      if program.withMemory(1 -> noun, 2 -> verb).run()().read(0) == 19690720
    } yield 100 * noun + verb

    matchingParams(0)
  }
}
