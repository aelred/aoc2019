package aoc2019.day7

import aoc2019.Solution
import aoc2019.program.Program

object Part1 extends Solution[Program] {

  def solution: Long = {
    val program = input

    val allPhaseSettings = (1L to 4L).permutations

    val outputs = allPhaseSettings.map(trySetting(program, _))

    outputs.max
  }

  private def trySetting(program: Program, phaseSettings: Seq[Long]): Long = {
    val amplifiers = phaseSettings.map(new Amplifier(program, _))

    var input = 0L

    for (amplifier <- amplifiers) {
      amplifier.run(input) match {
        case Some(output) => input = output
        case None         => throw new Exception("Program exited early")
      }
    }

    input
  }

}
