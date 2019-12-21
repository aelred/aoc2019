package aoc2019.day4

import aoc2019.Solution

object Part1 extends Solution[Range] {

  def solution: Int = {
    val range = input

    val possiblePasswords = for {
      password <- range
      passStr = password.toString

      digitCounts = passStr.toSeq.groupBy(identity)

      adjacentDigitsTheSame = digitCounts.values.exists(_.length >= 2)
      if adjacentDigitsTheSame

      neverDecreases = passStr.toSeq.sorted.unwrap == passStr
      if neverDecreases

    } yield password

    possiblePasswords.length
  }
}
