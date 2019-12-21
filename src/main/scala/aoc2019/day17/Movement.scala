package aoc2019.day17

sealed trait Movement {
  override def toString: String = this match {
    case TurnLeft() => "L"
    case TurnRight() => "R"
    case Forward() => "1"
  }
}

case class TurnLeft() extends Movement
case class TurnRight() extends Movement
case class Forward() extends Movement
