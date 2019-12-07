package aoc2019.day3

object Part2 extends WireSolution {

  def solution: Int = {
    val visits1 = visitedLocations(input(0))
    val visits2 = visitedLocations(input(1))

    val locations1 = visits1.map(_.location)
    val locations2 = visits2.map(_.location)

    val intersections = locations1 & locations2

    val combinedSteps = intersections.map(location => {
      val steps1 = visits1.find(visit => visit.location == location).get.steps
      val steps2 = visits2.find(visit => visit.location == location).get.steps
      steps1 + steps2
    })

    combinedSteps.min
  }
}
