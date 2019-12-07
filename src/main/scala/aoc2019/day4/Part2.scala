package aoc2019.day4

import aoc2019.Solution

object Part2 extends Solution[Range] {

  def solution: Int = {
    val range = line

    val possiblePasswords = for {
      password <- range
      passStr = password.toString

      digitCounts = passStr.toSeq.groupBy(identity)

      exactlyTwoAdjacentDigitsTheSame = digitCounts.values.exists(_.length == 2)
      if exactlyTwoAdjacentDigitsTheSame

      neverDecreases = passStr.toSeq.sorted.unwrap == passStr
      if neverDecreases

    } yield password

    possiblePasswords.length
  }
}
