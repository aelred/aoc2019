package aoc2019.day7

import aoc2019.program.Program

import scala.collection.mutable

class Amplifier(program: Program, phase: Long) {

  private val inputs: mutable.Queue[Long] = mutable.Queue(phase)

  private val execution = program.start(() => inputs.dequeue())

  def run(input: Long): Option[Long] = {
    inputs.enqueue(input)
    execution.continue()
  }
}
