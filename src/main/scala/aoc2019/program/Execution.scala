package aoc2019.program

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class Execution private (memory: mutable.Map[Long, Long], input: () => Long) {

  private var instructionPointer: Long = 0
  private var relativeBase: Long = 0
  private var halted: Boolean = false

  private class HaltException extends Exception

  def apply(f: (() => Long) => Unit): Unit = {
    def next() = {
      continue() match {
        case Some(value) => value
        case None => throw new HaltException()
      }
    }

    while (!halted) {
      try {
        f(() => next())
      } catch {
        case _: HaltException => return
      }
    }
  }

  def continue(): Option[Long] = {
    if (halted) return None

    while (true) {
      val (opCode, parameterModes) = readOpCode(consume())

      def read(): Long = {
        parameterModes.next match {
          case PositionMode()  => memory(consume())
          case ImmediateMode() => consume()
          case RelativeMode()  => memory(consume() + relativeBase)
        }
      }

      def write(newValue: Long): Unit = {
        parameterModes.next match {
          case PositionMode()  => memory(consume()) = newValue
          case ImmediateMode() => throw new Exception("Writing in immediate mode")
          case RelativeMode()  => memory(consume() + relativeBase) = newValue
        }
      }

      opCode match {
        case Add()                => write(read() + read())
        case Multiply()           => write(read() * read())
        case Input()              => write(input())
        case Output()             => return Some(read())
        case JumpIfTrue()         => if (read() != 0) instructionPointer = read() else read()
        case JumpIfFalse()        => if (read() == 0) instructionPointer = read() else read()
        case LessThan()           => write(if (read() < read()) 1 else 0)
        case Equals()             => write(if (read() == read()) 1 else 0)
        case AdjustRelativeBase() => relativeBase += read()
        case Halt()               => {
          halted = true
          return None
        }
      }
    }

    throw new Exception("Didn't halt")
  }

  private def consume(): Long = {
    val value = memory(instructionPointer)
    instructionPointer += 1
    value
  }

  private def readOpCode(value: Long): (OpCode, Iterator[ParameterMode]) = {
    val string = value.toString

    val (paramsStr, opCodeStr) = string.splitAt(string.length - 2)

    val zeros = LazyList.continually('0')

    val parameterModes = LazyList.concat(paramsStr.toSeq.reverse, zeros) map {
      case '0' => PositionMode()
      case '1' => ImmediateMode()
      case '2' => RelativeMode()
    }

    val opCodeInt = opCodeStr.toInt

    val opCode = opCodeInt match {
      case 1 => Add()
      case 2 => Multiply()
      case 3 => Input()
      case 4 => Output()
      case 5 => JumpIfTrue()
      case 6 => JumpIfFalse()
      case 7 => LessThan()
      case 8 => Equals()
      case 9 => AdjustRelativeBase()
      case 99 => Halt()
      case _ => throw new Exception(s"Unexpected opcode $opCodeInt")
    }

    (opCode, parameterModes.iterator)
  }
}

object Execution {
  def apply(memory: mutable.Seq[Long], input: () => Long): Execution = {
    val memoryIndices = memory.zipWithIndex map { case (x, addr) =>
      (addr.toLong, x)
    }

    val memoryMap = mutable.Map.from(memoryIndices).withDefaultValue(0)

    new Execution(memoryMap, input)
  }
}
