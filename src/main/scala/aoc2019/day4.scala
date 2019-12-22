package aoc2019

object day4 extends Solution[Range] {

  def part1: Int = {
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

  def part2: Int = {
    val range = input

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
