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

  def withMemory(setMemory: (Int, Long)*): Program = {
    val memory = initialMemory.toBuffer

    for ((address, value) <- setMemory) {
      memory(address) = value
    }

    new Program(memory.toSeq)
  }

  def executeAndReturnMemory: Seq[Long] = {
    val memory = initialMemory.toBuffer
    execute(memory, input=noInput, output = _ => {})
    memory.toSeq
  }

  def start(input: () => Long = noInput): Execution = {
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

  private val noInput = () => throw new Exception("No input")
}

