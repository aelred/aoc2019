package aoc2019.day17

sealed trait MoveFunc {
  override def toString: String = this match {
    case MoveFunc.A() => "A"
    case MoveFunc.B() => "B"
    case MoveFunc.C() => "C"
  }
}

object MoveFunc {
  case class A() extends MoveFunc
  case class B() extends MoveFunc
  case class C() extends MoveFunc
}
