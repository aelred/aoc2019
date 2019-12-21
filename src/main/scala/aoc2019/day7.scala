package aoc2019

import aoc2019.program.Program

import scala.collection.mutable

object day7 extends Solution[Program] {

  class Amplifier(program: Program, phase: Long) {

    private val inputs: mutable.Queue[Long] = mutable.Queue(phase)

    private val execution = program.start(() => inputs.dequeue())

    def run(input: Long): Option[Long] = {
      inputs.enqueue(input)
      execution.continue()
    }
  }

  def part1: Long = {
    val program = input

    val allPhaseSettings = (1L to 4L).permutations

    val outputs = allPhaseSettings.map(trySetting(program, _, identity))

    outputs.max
  }

  def part2: Long = {
    val program = input

    val allPhaseSettings = (5L to 9L).permutations

    val outputs = allPhaseSettings.map(trySetting(program, _, LazyList.continually(_).flatten))

    outputs.max
  }

  private def trySetting(program: Program, phaseSettings: Seq[Long], f: Seq[Amplifier] => Seq[Amplifier]): Long = {
    val amplifiers = phaseSettings.map(new Amplifier(program, _))

    var input = 0L

    for (amplifier <- amplifiers) {
      amplifier.run(input) match {
        case Some(output) => input = output
        case None         => return input
      }
    }

    input
  }
}
