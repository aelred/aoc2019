package aoc2019.program

import scala.collection.mutable

class Program(initialMemory: Seq[Long]) {

  def execute(input: Long*): Seq[Long] = {
    val memory = initialMemory.toBuffer

    val inputIterator = input.iterator
    val outputs = mutable.Buffer[Long]()

    execute(memory, inputIterator.next, outputs.append(_))

    outputs.toSeq
  }

  def executeWithNounAndVerb(noun: Long, verb: Long): Long = {
    val memory = initialMemory.toBuffer
    memory(1) = noun
    memory(2) = verb

    execute(memory, input = () => throw new Exception("No input"), output = _ => {})

    memory(0)
  }

  def start(input: () => Long): Execution = {
    Execution(mutable.Seq.from(initialMemory), input)
  }

  private def execute(memory: mutable.Seq[Long], input: () => Long, output: Long => Unit): Unit = {
    val execution = Execution(memory, input)

    while (true) {
      execution.continue() match {
        case Some(out) => output(out)
        case None => return
      }
    }
  }
}

