package aoc2019.program

sealed trait OpCode
case class Add() extends OpCode
case class Multiply() extends OpCode
case class Input() extends OpCode
case class Output() extends OpCode
case class JumpIfTrue() extends OpCode
case class JumpIfFalse() extends OpCode
case class LessThan() extends OpCode
case class Equals() extends OpCode
case class AdjustRelativeBase() extends OpCode
case class Halt() extends OpCode
