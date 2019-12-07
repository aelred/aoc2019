package aoc2019.day7

import aoc2019.Solution
import aoc2019.program.Program

object Part2 extends Solution[Program] {

  def solution: Int = {
    val program = line

    val allPhaseSettings = (5 to 9).permutations

    val outputs = allPhaseSettings.map(trySetting(program, _))

    outputs.max
  }

  private def trySetting(program: Program, phaseSettings: Seq[Int]): Int = {
    val amplifiers = phaseSettings.map(new Amplifier(program, _))

    val amplifiersLoop = LazyList.continually(amplifiers).flatten

    var input = 0

    for (amplifier <- amplifiersLoop) {
      amplifier.run(input) match {
        case Some(output) => input = output
        case None         => return input
      }
    }

    throw new Exception("Exited infinite loop somehow...")
  }

}
