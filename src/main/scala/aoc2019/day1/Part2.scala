package aoc2019.day1

import aoc2019.Solution

object Part2 extends Solution[Seq[Int]] {

  def fuelForMass(mass: Int): Int = {
    val fuelNeeded = (mass / 3) - 2

    if (fuelNeeded <= 0) {
      0
    } else {
      fuelNeeded + fuelForMass(fuelNeeded)
    }
  }

  def solution: Int = input.map(fuelForMass).sum
}
