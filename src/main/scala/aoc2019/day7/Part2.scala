package aoc2019.day7

import aoc2019.Solution
import aoc2019.program.Program

object Part2 extends Solution[Program] {

  def solution: Long = {
    val program = input

    val allPhaseSettings = (5L to 9L).permutations

    val outputs = allPhaseSettings.map(trySetting(program, _))

    outputs.max
  }

  private def trySetting(program: Program, phaseSettings: Seq[Long]): Long = {
    val amplifiers = phaseSettings.map(new Amplifier(program, _))

    val amplifiersLoop = LazyList.continually(amplifiers).flatten

    var input = 0L

    for (amplifier <- amplifiersLoop) {
      amplifier.run(input) match {
        case Some(output) => input = output
        case None         => return input
      }
    }

    throw new Exception("Exited infinite loop somehow...")
  }

}
