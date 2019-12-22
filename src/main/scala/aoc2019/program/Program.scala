package aoc2019.program

import aoc2019.parser.Parser
import aoc2019.parser.Parser.long

import scala.collection.mutable

class Program(initialMemory: Seq[Long]) {

  def execute(input: Long*): Seq[Long] = {
    val inputIterator = input.iterator
    val outputs = mutable.Buffer[Long]()

    run(inputIterator.next) { next =>
      outputs.append(next())
    }

    outputs.toSeq
  }

  def run(input: () => Long = noInput)(f: (() => Long) => Unit = noAction): Execution = runWhile(() => true)(input)(f)

  def runWhile(condition: () => Boolean)(input: () => Long = noInput)(f: (() => Long) => Unit = noAction): Execution = {
    val execution = start(input)

    class HaltException extends Exception

    def next() = {
      execution.continue() match {
        case Some(value) => value
        case None => throw new HaltException()
      }
    }

    do {
      try {
        f(next)
      } catch {
        case _: HaltException => return execution
      }
    } while (condition())

    execution
  }

  def withMemory(setMemory: (Int, Long)*): Program = {
    val memory = initialMemory.toBuffer

    for ((address, value) <- setMemory) {
      memory(address) = value
    }

    new Program(memory.toSeq)
  }

  def start(input: () => Long = noInput): Execution = {
    Execution(initialMemory.toBuffer, input)
  }

  private val noInput = () => throw new Exception("No input")
  private val noAction: (() => Long) => Unit = next => next()
}

object Program {
  implicit val parser: Parser[Program] = long.separatedBy(",") >> { new Program(_) }
}

