package aoc2019.program

sealed trait ParameterMode
case class PositionMode() extends ParameterMode
case class ImmediateMode() extends ParameterMode
