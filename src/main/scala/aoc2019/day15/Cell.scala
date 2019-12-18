package aoc2019.day15

sealed trait Cell
case class Wall() extends Cell
case class Clear() extends Cell
case class Oxygen() extends Cell
