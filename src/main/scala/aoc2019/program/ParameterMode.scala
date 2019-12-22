package aoc2019.program

sealed trait ParameterMode

object ParameterMode {
  case class Position() extends ParameterMode
  case class Immediate() extends ParameterMode
  case class Relative() extends ParameterMode
}
