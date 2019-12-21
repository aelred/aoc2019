package aoc2019.day17

import aoc2019.Solution
import aoc2019.program.Program

object Part1 extends BaseSolution {
  def solution: Int = {
    val program = input
    val (scaffolds, _) = createScaffoldSet(program)

    val leftEdge = scaffolds.map(_.x).min
    val topEdge = scaffolds.map(_.y).min

    val intersections = scaffolds.filter { pos =>
        pos.neighbours.forall(scaffolds.contains)
    }

    intersections.map(pos => (pos.x - leftEdge) * (pos.y - topEdge)).sum
  }
}
