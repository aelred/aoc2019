package aoc2019

object day1 extends Solution[List[Int]] {

  def part1: Int = input
      .map(x => (x / 3) - 2)
      .sum

  def part2: Int = input.map(fuelForMass).sum

  def fuelForMass(mass: Int): Int = {
    val fuelNeeded = (mass / 3) - 2

    if (fuelNeeded <= 0) {
      0
    } else {
      fuelNeeded + fuelForMass(fuelNeeded)
    }
  }
}
