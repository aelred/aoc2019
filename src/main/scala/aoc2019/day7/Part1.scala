package aoc2019.day7

import aoc2019.Solution
import aoc2019.program.Program

object Part1 extends Solution[Program] {

  def solution: Int = {
    val program = line
    val allPhaseSettings = (0 to 4).permutations

    val outputs = for {
      phaseSettings  <- allPhaseSettings

      output = phaseSettings.foldLeft(0) { (output, phaseSetting) =>
        program.execute(phaseSetting, output)(0)
      }

    } yield output

    outputs.max
  }
}
