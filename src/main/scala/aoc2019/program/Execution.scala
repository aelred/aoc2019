package aoc2019.program

import scala.collection.mutable

class Execution private (var instructionPointer: Int, memory: mutable.Seq[Int], input: () => Int) {

  def continue(): Option[Int] = {
    while (true) {
      val (opCode, parameterModes) = readOpCode(consume())

      def read(): Int = {
        parameterModes.next match {
          case PositionMode()  => memory(consume())
          case ImmediateMode() => consume()
        }
      }

      def write(newValue: Int): Unit = {
        parameterModes.next match {
          case PositionMode()  => memory(consume()) = newValue
          case ImmediateMode() => throw new Exception("Writing in immediate mode")
        }
      }

      opCode match {
        case Add()         => write(read() + read())
        case Multiply()    => write(read() * read())
        case Input()       => write(input())
        case Output()      => return Some(read())
        case JumpIfTrue()  => if (read() != 0) instructionPointer = read() else read()
        case JumpIfFalse() => if (read() == 0) instructionPointer = read() else read()
        case LessThan()    => write(if (read() < read()) 1 else 0)
        case Equals()      => write(if (read() == read()) 1 else 0)
        case Halt()        => return None
      }
    }

    throw new Exception("Didn't halt")
  }

  private def consume(): Int = {
    val value = memory(instructionPointer)
    instructionPointer += 1
    value
  }

  private def readOpCode(value: Int): (OpCode, Iterator[ParameterMode]) = {
    val string = value.toString

    val (paramsStr, opCodeStr) = string.splitAt(string.length - 2)

    val zeros = LazyList.continually('0')

    val parameterModes = LazyList.concat(paramsStr.toSeq.reverse, zeros) map {
      case '0' => PositionMode()
      case '1' => ImmediateMode()
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
      case 99 => Halt()
      case _ => throw new Exception(s"Unexpected opcode $opCodeInt")
    }

    (opCode, parameterModes.iterator)
  }
}

object Execution {
  def apply(memory: mutable.Seq[Int], input: () => Int): Execution = {
    new Execution(0, memory, input)
  }
}
