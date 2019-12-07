package aoc2019.day2

import aoc2019.Solution
import aoc2019.program.Program

object Part2 extends Solution[Program] {

  def solution: Int = {
    val program = line

    val matchingParams = for {
      noun <- 0 to 100
      verb <- 0 to 100
      x = program.execute(noun, verb)
      _ = println(s"$noun $verb = $x")
      if program.execute(noun, verb) == 19690720
    } yield 100 * noun + verb

    matchingParams(0)
  }
}
