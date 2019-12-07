package aoc2019.day7

import aoc2019.program.Program

import scala.collection.mutable

class Amplifier(program: Program, phase: Int) {

  private val inputs: mutable.Queue[Int] = mutable.Queue(phase)

  private val execution = program.start(() => inputs.dequeue())

  def run(input: Int): Option[Int] = {
    inputs.enqueue(input)
    execution.continue()
  }
}
