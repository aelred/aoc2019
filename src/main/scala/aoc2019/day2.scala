package aoc2019

import aoc2019.program.Program

object day2 extends Solution[Program] {

  def part1: Long = {
    input.withMemory(1 -> 12, 2 -> 2).run()().read(0)
  }

  def part2: Int = {
    val program = input

    val matchingParams = for {
      noun <- 0 to 100
      verb <- 0 to 100
      if program.withMemory(1 -> noun, 2 -> verb).run()().read(0) == 19690720
    } yield 100 * noun + verb

    matchingParams(0)
  }
}
