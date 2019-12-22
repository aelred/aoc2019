package aoc2019.program

import aoc2019.program.OpCode._
import aoc2019.program.ParameterMode.{Immediate, Position, Relative}

import scala.collection.mutable

class Execution private (memory: mutable.Map[Long, Long], input: () => Long) {

  private var instructionPointer: Long = 0
  private var relativeBase: Long = 0

  def read(address: Long): Long = memory(address)

  def continue(): Option[Long] = {
    while (true) {
      val (opCode, parameterModes) = readOpCode(consume())

      def read(): Long = {
        parameterModes.next match {
          case Position()  => memory(consume())
          case Immediate() => consume()
          case Relative()  => memory(consume() + relativeBase)
        }
      }

      def write(newValue: Long): Unit = {
        parameterModes.next match {
          case Position()  => memory(consume()) = newValue
          case Immediate() => throw new Exception("Writing in immediate mode")
          case Relative()  => memory(consume() + relativeBase) = newValue
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
        case Halt()               => return None
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
      case '0' => Position()
      case '1' => Immediate()
      case '2' => Relative()
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
