package aoc2019.day13

import scala.collection.mutable

sealed trait Tile {
  def char: Char = this match {
    case Empty() => ' '
    case Wall() => '█'
    case Block() => '#'
    case Paddle() => '='
    case Ball() => 'O'
  }
}

case class Empty() extends Tile
case class Wall() extends Tile
case class Block() extends Tile
case class Paddle() extends Tile
case class Ball() extends Tile

object Tile {
  def apply(num: Long): Tile = {
    num match {
      case 0 => Empty()
      case 1 => Wall()
      case 2 => Block()
      case 3 => Paddle()
      case 4 => Ball()
    }
  }
}
