package aoc2019.program

import scala.collection.mutable

class Program(initialMemory: Seq[Int]) {

  def execute(input: Int*): Seq[Int] = {
    val memory = initialMemory.toBuffer

    val inputIterator = input.iterator
    val outputs = mutable.Buffer[Int]()

    execute(memory, inputIterator.next, outputs.append(_))

    outputs.toSeq
  }

  def executeWithNounAndVerb(noun: Int, verb: Int): Int = {
    val memory = initialMemory.toBuffer
    memory(1) = noun
    memory(2) = verb

    execute(memory, input = () => throw new Exception("No input"), output = _ => {})

    memory(0)
  }

  def start(input: () => Int): Execution = {
    Execution(mutable.Seq.from(initialMemory), input)
  }

  private def execute(memory: mutable.Seq[Int], input: () => Int, output: Int => Unit): Unit = {
    val execution = Execution(memory, input)

    while (true) {
      execution.continue() match {
        case Some(out) => output(out)
        case None => return
      }
    }
  }
}

